#include <memory_resource>
#include <vector>
#include <ranges>
#include <sstream>

#include <gtest/gtest.h>
#include <fox/stack_tree.hpp>

#ifdef _MSC_VER
#pragma warning( push )
#pragma warning( disable : 4834 )
#endif

namespace fox
{
	// This is needed for consistent stack-trace level generation for unit-tests.
#ifdef _MSC_VER
#define NOINLINE __declspec(noinline)
#else
#define NOINLINE __attribute__((noinline))
#endif


	template<class T>
	class stacktree_test : public ::testing::Test
	{
	protected:
		NOINLINE void SetUp() override
		{
			stacktrace_0 = st_func0();
			stacktrace_1 = st_func1();
		}

	private:
		NOINLINE [[nodiscard]] static std::stacktrace st_func0()
		{
			return std::stacktrace::current();
		}

		NOINLINE [[nodiscard]] static std::stacktrace st_func1()
		{
			return std::stacktrace::current();
		}

	public:
		using type = T;

		std::pmr::monotonic_buffer_resource resource_0;
		std::pmr::monotonic_buffer_resource resource_1;
		std::pmr::polymorphic_allocator<> alloc_0{ std::addressof(resource_0) };
		std::pmr::polymorphic_allocator<> alloc_1{ std::addressof(resource_1) };

		std::stacktrace stacktrace_0;
		std::stacktrace stacktrace_1;

		template<class Allocator>
		void validate_stacktrace_0(const fox::basic_stacktree<T, Allocator>& st)
		{
			ASSERT_FALSE(st.empty());
			ASSERT_NE(st.begin(), st.end());

			auto b = st.begin();
			auto e = st.end();

			for (const auto& v : stacktrace_0 | std::views::reverse)
			{
				ASSERT_EQ(std::distance(b, e), 1);
				ASSERT_EQ(static_cast<std::stacktrace_entry>(*b), v);

				e = b.end_child();
				b = b.begin_child();
			}
		}

		template<class Allocator>
		void validate_stacktrace_1(const fox::basic_stacktree<T, Allocator>& st)
		{
			ASSERT_FALSE(st.empty());
			ASSERT_NE(st.begin(), st.end());

			auto b = st.begin();
			auto e = st.end();

			for (const auto& v : stacktrace_1 | std::views::reverse)
			{
				ASSERT_EQ(std::distance(b, e), 1);
				ASSERT_EQ(static_cast<std::stacktrace_entry>(*b), v);

				e = b.end_child();
				b = b.begin_child();
			}
		}

		template<class Allocator>
		void validate_stacktrace_01(const fox::basic_stacktree<T, Allocator>& st)
		{
			ASSERT_FALSE(st.empty());
			ASSERT_NE(st.begin(), st.end());

			auto b = st.begin();
			auto e = st.end();

			for (const auto& v : stacktrace_1 | std::views::drop(2) | std::views::reverse)
			{
				ASSERT_EQ(std::distance(b, e), 1);
				ASSERT_EQ(static_cast<std::stacktrace_entry>(*b), v);

				e = b.end_child();
				b = b.begin_child();
			}

			std::set last{ *(stacktrace_0.begin() + 1), *(stacktrace_1.begin() + 1) };

			EXPECT_TRUE(std::ranges::equal(std::begin(last), std::end(last), b, e));

			auto b0 = b.begin_child();
			auto e0 = b.end_child();

			auto b1 = (b+1).begin_child();
			auto e1 = (b+1).end_child();

			ASSERT_EQ(std::distance(b0, e0), 1);
			ASSERT_EQ(std::distance(b1, e1), 1);
			ASSERT_EQ(static_cast<std::stacktrace_entry>(*b0), *stacktrace_0.begin());
			ASSERT_EQ(static_cast<std::stacktrace_entry>(*b1), *stacktrace_1.begin());
		}

		template<class... Args>
		fox::basic_stacktree<T, decltype(alloc_0)> construct_0(Args&&... args)
		{
			auto out = fox::basic_stacktree<T, std::remove_cvref_t<decltype(alloc_0)>>
				( std::forward<Args>(args)..., alloc_0 );

			EXPECT_EQ(out.get_allocator(), alloc_0);
			return out;
		}

		template<class... Args>
		fox::basic_stacktree<T, decltype(alloc_0)> construct_1(Args&&... args)
		{
			auto out = fox::basic_stacktree<T, std::remove_cvref_t<decltype(alloc_1)>>
				( std::forward<Args>(args)..., alloc_1 );

			EXPECT_EQ(out.get_allocator(), alloc_1);
			return out;
		}

	};

	using types = testing::Types<void, int>;
	TYPED_TEST_SUITE(stacktree_test, types);

	TYPED_TEST(stacktree_test, constructor_default)
	{
		fox::stacktree<TypeParam> a;

		EXPECT_TRUE(a.empty());
		EXPECT_EQ(a.begin(), a.end());
	}

	TYPED_TEST(stacktree_test, constructor_allocator)
	{
		auto a = this->construct_0();

		EXPECT_TRUE(a.empty());
		EXPECT_EQ(a.begin(), a.end());
		EXPECT_EQ(a.get_allocator().resource(), std::addressof(this->resource_0));
	}

	TYPED_TEST(stacktree_test, constructor_copy_constructor)
	{
		fox::stacktree<TypeParam> a{ this->stacktrace_0 };
		this->validate_stacktrace_0(a);
		fox::stacktree<TypeParam> b = a;
		this->validate_stacktrace_0(a);
		this->validate_stacktrace_0(b);

		EXPECT_EQ(a, b);
	}

	TYPED_TEST(stacktree_test, constructor_move_construct)
	{
		fox::stacktree<TypeParam> a{ this->stacktrace_0 };
		this->validate_stacktrace_0(a);
		fox::stacktree<TypeParam> b = std::move(a);
		this->validate_stacktrace_0(b);

		EXPECT_TRUE(a.empty());
		EXPECT_EQ(a.begin(), a.end());

		EXPECT_NE(a, b);
	}

	TYPED_TEST(stacktree_test, constructor_copy_constructor_allocator)
	{
		auto a = this->construct_0(std::from_range, std::vector{ this->stacktrace_0 });
		this->validate_stacktrace_0(a);
		auto b = this->construct_1(a);
		this->validate_stacktrace_0(a);
		this->validate_stacktrace_0(b);

		EXPECT_EQ(a, b);
		EXPECT_NE(a.get_allocator().resource(), b.get_allocator().resource());
	}

	TYPED_TEST(stacktree_test, constructor_move_constructor_allocator)
	{
		auto a = this->construct_0(std::from_range, std::vector{ this->stacktrace_0 });
		this->validate_stacktrace_0(a);
		auto b = this->construct_1(std::move(a));
		this->validate_stacktrace_0(b);

		//  In an element-wise move other is not guaranteed to be empty after the move.

		// EXPECT_TRUE(a.empty());
		// EXPECT_EQ(a.begin(), a.end());

		// EXPECT_NE(a, b);
		EXPECT_NE(a.get_allocator().resource(), b.get_allocator().resource());
	}

	TYPED_TEST(stacktree_test, constructor_iterator_pair)
	{
		std::vector v{ this->stacktrace_0, this->stacktrace_1 };
		auto a = this->construct_0(std::begin(v), std::end(v));
		this->validate_stacktrace_01(a);
	}

	TYPED_TEST(stacktree_test, constructor_initializer_list)
	{
		std::initializer_list il{ this->stacktrace_0, this->stacktrace_1 };
		auto a = this->construct_0(il);
		this->validate_stacktrace_01(a);
	}

	TYPED_TEST(stacktree_test, constructor_from_range)
	{
		std::vector v{ this->stacktrace_0, this->stacktrace_1 };
		auto a = this->construct_0(std::from_range, v);
		this->validate_stacktrace_01(a);
	}

	TYPED_TEST(stacktree_test, assignment_op_copy)
	{
		fox::stacktree<TypeParam> a{ this->stacktrace_0 };
		this->validate_stacktrace_0(a);
		fox::stacktree<TypeParam> b{this->stacktrace_1};
		this->validate_stacktrace_1(b);
		b = a;
		this->validate_stacktrace_0(a);
		this->validate_stacktrace_0(b);

		EXPECT_EQ(a, b);
	}

	TYPED_TEST(stacktree_test, assignment_op_move)
	{
		fox::stacktree<TypeParam> a{ this->stacktrace_0 };
		this->validate_stacktrace_0(a);
		fox::stacktree<TypeParam> b{ this->stacktrace_1 };
		this->validate_stacktrace_1(b);
		b = std::move(a);
		this->validate_stacktrace_0(b);

		EXPECT_TRUE(a.empty());
		EXPECT_EQ(a.begin(), a.end());

		EXPECT_NE(a, b);
	}

	TYPED_TEST(stacktree_test, assignment_op_il)
	{
		fox::stacktree<TypeParam> b{ this->stacktrace_1 };
		this->validate_stacktrace_1(b);
		b = std::initializer_list<std::stacktrace>{ this->stacktrace_0 };
		this->validate_stacktrace_0(b);
	}

	TYPED_TEST(stacktree_test, assign_il)
	{
		fox::stacktree<TypeParam> b{ this->stacktrace_1 };
		this->validate_stacktrace_1(b);
		b.assign(std::initializer_list<std::stacktrace>{ this->stacktrace_0 });
		this->validate_stacktrace_0(b);
	}

	TYPED_TEST(stacktree_test, assign_iterator_pair)
	{
		fox::stacktree<TypeParam> b{ this->stacktrace_1 };
		this->validate_stacktrace_1(b);
		auto il = std::initializer_list<std::stacktrace>{ this->stacktrace_0 };
		b.assign(std::begin(il), std::end(il));
		this->validate_stacktrace_0(b);
	}

	TYPED_TEST(stacktree_test, assign_range)
	{
		fox::stacktree<TypeParam> b{ this->stacktrace_1 };
		this->validate_stacktrace_1(b);
		b.assign_range(std::initializer_list<std::stacktrace>{ this->stacktrace_0 });
		this->validate_stacktrace_0(b);
	}

	TYPED_TEST(stacktree_test, get_allocator)
	{
		{
			auto a = this->construct_0();
			EXPECT_EQ(a.get_allocator(), this->alloc_0);
		}

		{
			fox::stacktree<TypeParam> a;
			EXPECT_EQ(a.get_allocator(), std::allocator<TypeParam>{});
		}
	}

	TYPED_TEST(stacktree_test, begin_end)
	{
		{
			auto a = this->construct_0();
			auto b = a.begin();
			auto e = a.end();

			EXPECT_TRUE((std::indirectly_writable<decltype(b), std::stacktrace_entry>));
			EXPECT_TRUE((std::indirectly_writable<decltype(e), std::stacktrace_entry>));

			EXPECT_TRUE(std::indirectly_readable<decltype(b)>);
			EXPECT_TRUE(std::indirectly_readable<decltype(e)>);
		}

		{
			auto a = this->construct_0();
			auto b = a.cbegin();
			auto e = a.cend();

			EXPECT_FALSE((std::indirectly_writable<decltype(b), std::stacktrace_entry>));
			EXPECT_FALSE((std::indirectly_writable<decltype(e), std::stacktrace_entry>));

			EXPECT_TRUE(std::indirectly_readable<decltype(b)>);
			EXPECT_TRUE(std::indirectly_readable<decltype(e)>);
		}

		{
			const auto a = this->construct_0();
			auto b = a.begin();
			auto e = a.end();

			EXPECT_FALSE((std::indirectly_writable<decltype(b), std::stacktrace_entry>));
			EXPECT_FALSE((std::indirectly_writable<decltype(e), std::stacktrace_entry>));

			EXPECT_TRUE(std::indirectly_readable<decltype(b)>);
			EXPECT_TRUE(std::indirectly_readable<decltype(e)>);
		}
	}

	TYPED_TEST(stacktree_test, iterator_operator_dereference)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();

		auto entry = *b;
		EXPECT_EQ(
			static_cast<std::stacktrace_entry>(entry), 
			*this->stacktrace_0.rbegin()
		);

		// Accessing invalid
		EXPECT_ANY_THROW(*e);
	}

	TYPED_TEST(stacktree_test, iterator_operator_arrow)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();

		auto entry = b->native_handle();
		EXPECT_EQ(
			entry,
			this->stacktrace_0.rbegin()->native_handle()
		);

		// Accessing invalid
		EXPECT_ANY_THROW(e->native_handle());
	}

	TYPED_TEST(stacktree_test, iterator_operator_subscript)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();

		auto entry = b[0];
		EXPECT_EQ(
			static_cast<std::stacktrace_entry>(entry),
			*this->stacktrace_0.rbegin()
		);

		EXPECT_NO_THROW(e[-1]);

		// Accessing invalid
		EXPECT_ANY_THROW(e[0]);
		EXPECT_ANY_THROW(b[1]);
		EXPECT_ANY_THROW(b[-1]);
	}

	TYPED_TEST(stacktree_test, iterator_operator_preincrement)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();
		++b;
		EXPECT_EQ(b, e);

		// Accessing invalid
		EXPECT_ANY_THROW(++b);
		EXPECT_ANY_THROW(++e);
	}

	TYPED_TEST(stacktree_test, iterator_operator_postincrement)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();
		auto v = b++;
		EXPECT_NE(b, v);
		EXPECT_EQ(v, e);

		// Accessing invalid
		EXPECT_ANY_THROW((void)v++);
		EXPECT_ANY_THROW((void)e++);
	}

	TYPED_TEST(stacktree_test, iterator_operator_predecrement)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();
		--e;
		EXPECT_EQ(b, e);

		EXPECT_ANY_THROW(--b);
	}

	TYPED_TEST(stacktree_test, iterator_operator_postdecrement)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();
		auto v = e--;
		EXPECT_NE(e, v);
		EXPECT_EQ(v, b);

		EXPECT_ANY_THROW((void)b--);
		EXPECT_ANY_THROW((void)v--);
	}

	TYPED_TEST(stacktree_test, iterator_operator_eqaulity)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();
		EXPECT_FALSE(b == e);
		EXPECT_TRUE(b == b);
	}

	TYPED_TEST(stacktree_test, iterator_operator_ineqaulity)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();
		EXPECT_TRUE(b != e);
		EXPECT_FALSE(b != b);
	}

	TYPED_TEST(stacktree_test, iterator_operator_add_assign)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();
		b += 1;
		EXPECT_EQ(b, e);

		// Accessing invalid
		EXPECT_ANY_THROW(b += 1);
		EXPECT_ANY_THROW(e += 2);
	}

	TYPED_TEST(stacktree_test, iterator_operator_add_0)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();
		auto d = b + 1;
		EXPECT_EQ(b + 1, e);
		EXPECT_EQ(d, e);
		EXPECT_EQ(b, e + (-1));

		// Accessing invalid
		EXPECT_ANY_THROW(d + 1);
		EXPECT_ANY_THROW(d + 2);
		EXPECT_ANY_THROW(b + 2);
		EXPECT_ANY_THROW(e + 1);
		EXPECT_ANY_THROW(e + (-2));
	}

	TYPED_TEST(stacktree_test, iterator_operator_add_1)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();
		auto d = 1 + b;
		EXPECT_EQ(1 + b, e);
		EXPECT_EQ(d, e);
		EXPECT_EQ(b, (-1) + e);

		// Accessing invalid
		EXPECT_ANY_THROW(1 + d);
		EXPECT_ANY_THROW(2 + d);
		EXPECT_ANY_THROW(2 + b);
		EXPECT_ANY_THROW(1 + e);
		EXPECT_ANY_THROW((-2) + e);
	}

	TYPED_TEST(stacktree_test, iterator_operator_substract_assign)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();
		e -= 1;
		EXPECT_EQ(b, e);

		// Accessing invalid
		EXPECT_ANY_THROW(b -= 1);
		EXPECT_ANY_THROW(e -= 2);
	}

	TYPED_TEST(stacktree_test, iterator_operator_sub)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();
		auto d = e - 1;
		EXPECT_EQ(b, e - 1);
		EXPECT_EQ(d, b);
		EXPECT_EQ(b - (-1), e);

		// Accessing invalid
		EXPECT_ANY_THROW(d - 1);
		EXPECT_ANY_THROW(d - 2);
		EXPECT_ANY_THROW(e - 2);
		EXPECT_ANY_THROW(b - 1);
		EXPECT_ANY_THROW(b - (-2));
	}

	TYPED_TEST(stacktree_test, iterator_operator_diff)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();

		EXPECT_EQ(b - b, 0);
		EXPECT_EQ(b - e, -1);
		EXPECT_EQ(e - b, 1);
		EXPECT_EQ(e - e, 0);
	}

	TYPED_TEST(stacktree_test, iterator_operator_less)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();

		EXPECT_TRUE(b < e);
		EXPECT_FALSE(b < b);
		EXPECT_FALSE(e < b);
		EXPECT_FALSE(e < e);
	}

	TYPED_TEST(stacktree_test, iterator_operator_less_eq)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();

		EXPECT_TRUE(b <= e);
		EXPECT_TRUE(b <= b);
		EXPECT_FALSE(e <= b);
		EXPECT_TRUE(e <= e);
	}

	TYPED_TEST(stacktree_test, iterator_operator_greater)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();

		EXPECT_FALSE(b > e);
		EXPECT_FALSE(b > b);
		EXPECT_TRUE(e > b);
		EXPECT_FALSE(e > e);
	}

	TYPED_TEST(stacktree_test, iterator_operator_greater_eq)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();

		EXPECT_FALSE(b >= e);
		EXPECT_TRUE(b >= b);
		EXPECT_TRUE(e >= b);
		EXPECT_TRUE(e >= e);
	}

	TYPED_TEST(stacktree_test, iterator_child)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();

		auto cb = b.begin_child();
		auto ce = b.end_child();

		EXPECT_NE(cb, ce);

		auto entry = *cb;
		EXPECT_EQ(
			static_cast<std::stacktrace_entry>(entry),
			*(this->stacktrace_0.rbegin() + 1)
		);
	}

	TYPED_TEST(stacktree_test, iterator_parent)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });

		auto b = a.begin();
		auto e = a.end();

		auto cb = b.begin_child();
		auto ce = b.end_child();

		auto pb = cb.begin_parent();
		auto pe = cb.end_parent();

		EXPECT_NE(pb, pe);
		EXPECT_EQ(pb, b);
		EXPECT_EQ(pe, e);
	}

	TYPED_TEST(stacktree_test, insert_0)
	{
		auto a = this->construct_0();
		a.insert(this->stacktrace_0,
			[this, i = this->stacktrace_0.rbegin()](typename decltype(a)::value_type& v) mutable
			{
				EXPECT_TRUE((v == *(i++)));
			}
		);

		this->validate_stacktrace_0(a);
	}

	TYPED_TEST(stacktree_test, insert_1)
	{
		auto a = this->construct_0();
		a.insert(this->stacktrace_0);
		this->validate_stacktrace_0(a);
	}

	TYPED_TEST(stacktree_test, insert_2)
	{
		auto a = this->construct_0();
		std::initializer_list v{ this->stacktrace_0, this->stacktrace_1 };
		a.insert(std::begin(v), std::end(v));
		this->validate_stacktrace_01(a);
	}

	TYPED_TEST(stacktree_test, insert_3)
	{
		auto a = this->construct_0();
		std::initializer_list v{ this->stacktrace_0, this->stacktrace_1 };
		a.insert(v);
		this->validate_stacktrace_01(a);
	}

	TYPED_TEST(stacktree_test, insert_range)
	{
		auto a = this->construct_0();
		std::initializer_list v{ this->stacktrace_0, this->stacktrace_1 };
		a.insert_range(v);
		this->validate_stacktrace_01(a);
	}

	TYPED_TEST(stacktree_test, erase_0)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });
		auto pos = a.begin().begin_child().begin_child();
		a.erase(pos);
		EXPECT_EQ((a.begin().begin_child().begin_child()), (a.begin().begin_child().end_child()));
	}

	TYPED_TEST(stacktree_test, erase_1)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });
		auto b = a.begin().begin_child().begin_child();
		auto e = a.begin().begin_child().end_child();
		a.erase(b, e);
		EXPECT_EQ((a.begin().begin_child().begin_child()), (a.begin().begin_child().end_child()));
	}

	TYPED_TEST(stacktree_test, empty)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });
		EXPECT_FALSE(a.empty());
		auto b = this->construct_1();
		EXPECT_TRUE(b.empty());
	}

	TYPED_TEST(stacktree_test, clear)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });
		EXPECT_FALSE(a.empty());
		a.clear();
		EXPECT_TRUE(a.empty());
		auto b = this->construct_1();
		EXPECT_TRUE(b.empty());
		a.clear();
		EXPECT_TRUE(b.empty());
	}

	TYPED_TEST(stacktree_test, op_eq)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });
		auto b = this->construct_1(std::initializer_list{ this->stacktrace_1 });
		auto c = this->construct_1(std::initializer_list{ this->stacktrace_1 });

		EXPECT_FALSE(a == b);
		EXPECT_FALSE(a == c);
		EXPECT_TRUE(b == c);
		EXPECT_TRUE(a == a);
		EXPECT_TRUE(b == b);
	}

	TYPED_TEST(stacktree_test, op_neq)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });
		auto b = this->construct_1(std::initializer_list{ this->stacktrace_1 });
		auto c = this->construct_1(std::initializer_list{ this->stacktrace_1 });

		EXPECT_TRUE(a != b);
		EXPECT_TRUE(a != c);
		EXPECT_FALSE(b != c);
		EXPECT_FALSE(a != a);
		EXPECT_FALSE(b != b);
	}

	TYPED_TEST(stacktree_test, swap)
	{
		auto a = this->construct_0(std::initializer_list{ this->stacktrace_0 });
		auto b = this->construct_1(std::initializer_list{ this->stacktrace_1 });

		auto a_old = a;
		auto b_old = b;

		a.swap(b);
		EXPECT_EQ(a_old, b);
		EXPECT_EQ(b_old, a);
		EXPECT_NE(a_old, a);
		EXPECT_NE(b_old, b);
	}
}

#ifdef _MSC_VER
#pragma warning( pop )
#endif