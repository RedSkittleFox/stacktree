#ifndef FOX_STACKTREE_HPP_
#define FOX_STACKTREE_HPP_
#pragma once

#include <map>
#include <memory>
#include <stacktrace>
#include <iterator>
#include <functional>
#include <algorithm>
#include <cassert>
#include <ranges>
#include <initializer_list>
#include <scoped_allocator>
#include <sstream>

#ifndef __cpp_lib_stacktrace
#error "Stacktree library requires stacktrace library to be present."
#endif

namespace fox
{
	/**
	 * \brief Extension of std::stacktrace_entry that carries value with it.
	 * \tparam T The type of the element
	 */
	template<class T>
	class stacktree_entry : public std::stacktrace_entry
	{
		T value_;

	public:
		/**
		 * \brief Default constructor. Creates an empty stacktree_entry
		 */
		stacktree_entry() : value_{} {}

		/**
		 * \brief Constructs the stacktree_entry from std::stacktrace_entry and the value
		 * \param se std::stracktrace_entry to initialzie stacktree_entry with
		 * \param value the value to initialize stacktree_entry with
		 */
		stacktree_entry(const std::stacktrace_entry& se, const T& value = {})
			: std::stacktrace_entry(se), value_(value) {}

		/**
		 * \brief Copy constructor. Constructs stacktree_entry from a copy.
		 * \param other another stacktree_entry to be used as a source to initialize the stacktree_entry with
		 */
		stacktree_entry(const stacktree_entry& other) = default;

		/**
		 * \brief Move constructor. Constructs stacktree_entry by moving contents from
		 * \param other other another stacktree_entry to be used as a source to initialize the stacktree_entry with
		 */
		stacktree_entry(stacktree_entry&& other)
			noexcept(std::is_nothrow_move_constructible_v<T> && std::is_nothrow_move_constructible_v<std::stacktrace_entry>) = default;


		/**
		 * \brief Copy assignment operator.
		 * \param other another stacktree_entry to be used as a source to initialize the stacktree_entry with
		 * \return *this
		 */
		stacktree_entry& operator=(const stacktree_entry& other) = default;

		/**
		 * \brief Replaces the underlying stacktrace_entry with the copy
		 * \param other another stacktrace_entry to be used as a source to initialize the stacktree_entry with
		 * \return *this
		 */
		stacktree_entry& operator=(const std::stacktrace_entry& other)
		{
			std::stacktrace_entry::operator=(other);
			value_ = {};
			return *this;
		}

		/**
		 * \brief Move assignment operator.
		 * \param other another stacktree_entry to be used as a source to initialize the stacktree_entry with
		 * \return *this
		 */
		stacktree_entry& operator=(stacktree_entry&& other)
			noexcept(std::is_nothrow_move_assignable_v<T> && std::is_nothrow_move_assignable_v<std::stacktrace_entry>) = default;

		/**
		 * \brief Destructs the stacktree_entry.
		 */
		~stacktree_entry() noexcept = default;

	public:
		/**
		 * \brief Converts stacktree_entry to stored-type reference.
		 */
		explicit operator const T&() const noexcept
		{
			return value_;
		}

		/**
		 * \brief Converts stacktree_entry to stored-type reference.
		 */
		explicit operator T& () noexcept
		{
			return value_;
		}

		/**
		 * \brief Getter for stored value.
		 * \return Reference to stored value.
		 */
		const T& value() const noexcept
		{
			return value_;
		}

		/**
		 * \brief Getter for stored value.
		 * \return Reference to stored value.
		 */
		T& value() noexcept
		{
			return value_;
		}
	};

	/**
	 * \brief Type trait checking if type T is an instantiation of std::basic_stacktree<Allocator>
	 */
	template<class T> struct is_basic_stacktrace : std::false_type {};

	/**
	 * \brief Type trait checking if type T is an instantiation of std::basic_stacktree<Allocator>
	 */
	template<class Allocator> struct is_basic_stacktrace<std::basic_stacktrace<Allocator>> : std::true_type {};

	/**
	 * \brief Type trait checking if type T is an instantiation of std::scoped_allocator_adaptor<Outer, Inner...>
	 */
	template<class T> struct is_scoped_allocator_adaptor : std::false_type {};

	/**
	 * \brief Type trait checking if type T is an instantiation of std::scoped_allocator_adaptor<Outer, Inner...>
	 */
	template<class Outer, class... Inner> struct is_scoped_allocator_adaptor<std::scoped_allocator_adaptor<Outer, Inner...>> : std::true_type {};

	/**
	 * \brief The basic_stacktree class template represents a tree structure with attached data composed of multiple basic_stacktraces
	 * \tparam T Type to be stored by basic_stacktree nodes.
	 * \tparam Allocator An allocator that is used to acquire/release memory and to construct/destroy the elements in that memory. 
	 */
	template<class T, std::copy_constructible Allocator>
		requires std::same_as<T, void> || std::copyable<T>
	class basic_stacktree
	{
	public:
		/**
		 * \brief std::stacktrace_entry if T is void or stacktree_entry<T>
		 */
		using value_type = 
			std::conditional_t<std::is_same_v<T, void>,
				std::stacktrace_entry,
				stacktree_entry<T>
		>;

		/**
		 * \brief std::stacktrace_entry
		 */
		using key_type = std::stacktrace_entry;

		/**
		 * \brief T
		 */
		using mapped_type = T;

		/**
		 * \brief Allocator
		 */
		using allocator_type = Allocator;

	private:
		struct node;

		using node_allocator_type = typename  std::allocator_traits<allocator_type>::template rebind_alloc<node>;

		// Check if the allocator is scoped-like - during the construction of the members/collection,
		// the parent object uses it's own allocator to allocate children. This library currently implements a multi-level
		// tree structure using nested containers.
		template<
			class inner_t = typename std::allocator_traits<allocator_type>::template rebind_alloc<int>,
			class outer_t = std::scoped_allocator_adaptor<typename std::allocator_traits<allocator_type>::template rebind_alloc<inner_t>>
		>
		static constexpr bool is_not_scoped_allocator_impl = requires
		{
			{ std::allocator_traits<outer_t>
				::construct(std::declval<outer_t>(), static_cast<std::vector<int, inner_t>*>(nullptr)) } -> std::same_as<void>;
		};

		static constexpr bool is_scoped_allocator = 
			is_scoped_allocator_adaptor<allocator_type>::value ||
			!is_not_scoped_allocator_impl<>
		;

		using scoped_node_allocator_type =
			std::conditional_t<
			is_scoped_allocator,
			node_allocator_type,
			std::scoped_allocator_adaptor<node_allocator_type>
			>;

		static_assert(std::is_constructible_v<node_allocator_type, std::add_lvalue_reference_t<std::add_const_t<allocator_type>>>,
			"Selected allocator type cannot be rebound (copy-construct) to allocator_type<node>. ");

		using children_container = std::vector<node, scoped_node_allocator_type >;

		struct node
		{
			using allocator_type = scoped_node_allocator_type;

			node() : node(allocator_type()) {}

			node(const allocator_type& alloc)
				: parent(nullptr), children(static_cast<scoped_node_allocator_type>(alloc)) {}

			node(const node& other) = default;

			node(node&& other) noexcept = default;

			node(node&& other, const allocator_type& alloc) noexcept
				:
				parent(std::exchange(other.parent, nullptr)),
				stacktrace(std::move(other.stacktrace)),
				children(std::move(other.children), static_cast<scoped_node_allocator_type>(alloc))
			{}

			node(const node& other, const allocator_type& alloc)
				:
				parent(other.parent),
				stacktrace(other.stacktrace),
				children(other.children, static_cast<scoped_node_allocator_type>(alloc))
			{}

			node& operator=(const node& other) = default;
			node& operator=(node&& other) noexcept = default;

			~node() noexcept = default;

			node(
				node* parent, 
				const value_type& stacktrace,
				const allocator_type& alloc = allocator_type()
				) :
				parent{parent},
				stacktrace{ stacktrace },
				children(static_cast<scoped_node_allocator_type>(alloc))
			{}

			node(
				node* parent,
				const value_type& stacktrace,
				const children_container& children,
				const allocator_type& alloc = allocator_type()
			) :
				parent{ parent },
				stacktrace{ stacktrace },
				children(children, static_cast<scoped_node_allocator_type>(alloc))
			{}

			node(
				node* parent,
				const value_type& stacktrace,
				children_container&& children,
				const allocator_type& alloc = allocator_type()
			) :
				parent{ parent },
				stacktrace{ stacktrace },
				children(std::move(children), static_cast<scoped_node_allocator_type>(alloc))
			{}

			node* parent = nullptr;
			value_type stacktrace;
			children_container children;

			[[nodiscard]] bool operator==(const node& rhs) const noexcept
			{
				return
					this->stacktrace == rhs.stacktrace &&
					this->children == rhs.children;
			}
		};

		static_assert(std::uses_allocator_v<node, scoped_node_allocator_type >);

		friend bool operator<(const node& lhs, const std::stacktrace_entry& rhs)
		{
			return lhs.stacktrace < rhs;
		}

		friend bool operator<(const std::stacktrace_entry& lhs, const node& rhs)
		{
			return lhs < rhs.stacktrace;
		}

		node root_node_;

	public:
		/**
		 * \brief Default constructor. Constructs an empty basic_stacktree with a default-constructed allocator.
		 */
		basic_stacktree()
		requires std::is_default_constructible_v<node>
			= default;

		/**
		 * \brief Constructs an empty basic_stacktree with the given allocator alloc.
		 * \param alloc allocator to use for all memory allocations of this container
		 */
		explicit basic_stacktree(const allocator_type& alloc)
			: root_node_(nullptr, {}, node_allocator_type{alloc}) {}

		/**
		 * \brief Copy constructor. Constructs the basic_stacktree with the copy of the contents of other.
		 * \param other another container to be used as source to initialize the elements of the container with
		 */
		basic_stacktree(const basic_stacktree& other) = default;

		/**
		 * \brief Move constructor. Constructs the basic_stacktree with the contents of other using move semantics. 
		 * \param other another container to be used as source to initialize the elements of the container with
		 */
		basic_stacktree(basic_stacktree&& other) noexcept(
			std::allocator_traits<allocator_type>::is_always_equal::value
			) = default;

		/**
		 * \brief Constructs the container with the copy of the contents of other, using alloc as the allocator.
		 * \param other another container to be used as source to initialize the elements of the container with
		 * \param alloc allocator to use for all memory allocations of this container
		 */
		basic_stacktree(
			const basic_stacktree& other,
			const allocator_type& alloc
		) : root_node_(
			nullptr, 
			other.root_node_.stacktrace,
			other.root_node_.children,
			scoped_node_allocator_type{ node_allocator_type{ alloc } }
		) {}

		/**
		 * \brief Allocator-extended move constructor.
		 * \param other another container to be used as source to initialize the elements of the container with
		 * \param alloc allocator to use for all memory allocations of this container
		 */
		basic_stacktree(
			basic_stacktree&& other,
			const allocator_type& alloc
		) : root_node_(
			std::move(other.root_node_),
			node_allocator_type{alloc}
		)
		{}

		/**
		 * \brief Constructs the basic_stacktree with the contents of the range [first, last).
		 * \tparam InputIt LegacyInputIterator which is indirect convertible to T
		 * \param first iterator to begin of the range to copy from
		 * \param last iterator to end of the range to copy from
		 * \param alloc allocator to use for all memory allocations of this container
		 */
		template<std::input_iterator InputIt>
		basic_stacktree(InputIt first, InputIt last, const allocator_type& alloc = allocator_type())
			requires is_basic_stacktrace<std::iter_value_t<InputIt>>::value
		: root_node_(nullptr, {}, node_allocator_type{ alloc })
		{
			this->assign(first, last);
		}

		/**
		 * \brief Constructs the container with the contents of the initializer list init.
		 * \tparam StacktraceAlloc Stacktrace's Allocator
		 * \param il initializer list to initialize the elements of the container with
		 * \param alloc allocator to use for all memory allocations of this container
		 */
		template<class StacktraceAlloc>
		basic_stacktree(
			std::initializer_list<std::basic_stacktrace<StacktraceAlloc>> il,
			const allocator_type& alloc = allocator_type()
		)
			: basic_stacktree(std::begin(il), std::end(il), alloc) {}

		/**
		 * \brief Constructs the container with the contents of the range rg.
		 * \tparam R input_range whose elements are convertible to T
		 * \param rg a container compatible range, that is, an input_range whose elements are convertible to T
		 * \param alloc allocator to use for all memory allocations of this container
		 */
		template<std::ranges::input_range R>
		basic_stacktree(
			std::from_range_t,
			R&& rg,
			const allocator_type alloc = allocator_type()
		)
		requires is_basic_stacktrace<std::ranges::range_value_t<R>>::value
			: basic_stacktree(std::begin(rg), std::end(rg), alloc) {}

		/**
		 * \brief Copy assignment operator.
		 * \param other another container to use as data source
		 * \return *this
		 */
		basic_stacktree& operator=(const basic_stacktree& other) = default;

		/**
		 * \brief Move assignment operator. Replaces the contents with those of other using move semantics.
		 * \param other another container to use as data source
		 * \return *this
		 */
		basic_stacktree& operator=(basic_stacktree&& other)
			noexcept(
				std::allocator_traits<allocator_type>::is_always_equal::value
				)
		= default;

		/**
		 * \brief Replaces the contents with those identified by initializer list il.
		 * \tparam StacktraceAlloc std::basic_stacktrace's allocator
		 * \param il initializer list to use as data source
		 * \return *this
		 */
		template<class StacktraceAlloc>
		basic_stacktree& operator=(std::initializer_list<std::basic_stacktrace<StacktraceAlloc>> il)
		{
			this->assign(il);

			return *this;
		}

		/**
		 * \brief Destructs the basic_stacktree.
		 */
		~basic_stacktree() = default;

	public:
		/**
		 * \brief Replaces the contents with the elements from the initializer list il.
		 * \tparam StacktraceAlloc std::basic_stacktrace's allocator
		 * \param il initializer list to copy the values from
		 */
		template<class StacktraceAlloc>
		void assign(std::initializer_list<std::basic_stacktrace<StacktraceAlloc>> il)
		{
			return this->assign(std::begin(il), std::end(il));
		}

		/**
		 * \brief Replaces the contents with copies of those in the range [first, last).
		 * \tparam InputIt LegacyInputIterator which is indirect convertible to T
		 * \param first iterator to begin of the range to copy from
		 * \param last iterator to end of the range to copy from
		 */
		template<std::input_iterator InputIt>
		void assign(InputIt first, InputIt last)
			requires is_basic_stacktrace<std::iter_value_t<InputIt>>::value
		{
			this->clear();
			this->insert(first, last);
		}

		/**
		 * \brief Replaces elements in the basic_stacktree with a copy of each element in rg.
		 * \tparam R input_range whose elements are convertible to T
		 * \param rg a container compatible range, that is, an input_range whose elements are convertible to T
		 */
		template<std::ranges::input_range R>
		void assign_range(R&& rg)
			requires is_basic_stacktrace<std::ranges::range_value_t<R>>::value
		{
			return this->assign(std::begin(rg), std::end(rg));
		}

		/**
		 * \brief Returns the allocator associated with the container.
		 * \return The associated allocator.
		 */
		[[nodiscard]] allocator_type get_allocator() const noexcept
		{
			if constexpr(!is_scoped_allocator)
				return allocator_type{ this->root_node_.children.get_allocator().outer_allocator() };
			else
				return allocator_type{ this->root_node_.children.get_allocator() };
		}

	private:
		template<class U>
		class iterator_implementation
		{
			template<class G, std::copy_constructible>
				requires std::same_as<G, void> || std::copyable<G>
			friend class basic_stacktree;

		public:
			using iterator_category = std::random_access_iterator_tag;
			using value_type = U;
			using difference_type = std::ptrdiff_t;
			using pointer = U*;
			using reference = U&;

		private:
			using node_type = std::conditional_t<
				std::is_const_v<std::remove_reference_t<U>>,
				std::add_const_t<node>,
				node
			>;

		private:
			node_type* node_;
			difference_type index_;
		private:
			void assert_children_in_range(difference_type modifier) const // Assert not set index, strong exception guarantee
			{
				difference_type sum = this->index_ + modifier;
				auto e = std::out_of_range("Attempting to access out of range element via iterator.");;

				if (modifier > 0 && sum < this->index_) // overflow
					throw e;

				if (modifier < 0 && sum > this->index_) // underflow
					throw e;

				if (node_ == nullptr || sum < 0 || sum > std::size(node_->children))
					throw e;
			}

			void assert_valid_access() const
			{
				if(node_ == nullptr || index_ >= std::ssize(node_->children))
					throw std::out_of_range("Attempting to access out of range element via iterator.");
			}

			void assert_same_domain(const iterator_implementation& other) const
			{
				if (this->node_ != other.node_)
					throw std::domain_error("Attempting to compare iterators belonging to different objects.");
			}

			iterator_implementation(node_type* node, difference_type index = 0) noexcept // begin
				: node_(node), index_(index) {}

			iterator_implementation(node_type* node, std::in_place_t end_tag) noexcept // begin
				: node_(node), index_((node == nullptr ? 0 : node->children.size())) {}
		public:
			iterator_implementation() noexcept
				: node_(nullptr), index_(0) {}

			iterator_implementation(const iterator_implementation<std::remove_const_t<U>>& other)
				requires std::is_const_v<U>
			: node_(other.node_), index_(other.index_) {}

			iterator_implementation(const iterator_implementation&) noexcept = default;
			iterator_implementation(iterator_implementation&&) noexcept = default;

			iterator_implementation& operator=(const iterator_implementation&) noexcept = default;
			iterator_implementation& operator=(iterator_implementation&&) noexcept = default;

			~iterator_implementation() noexcept = default;

		public:
			reference operator*() const
			{
				assert_valid_access();
				return node_->children[index_].stacktrace;
			}

			pointer operator->() const
			{
				assert_valid_access();
				return std::addressof(node_->children[index_].stacktrace);
			}

			reference operator[](difference_type n) const
			{
				return *(*this + n);
			}

		public:
			iterator_implementation& operator++()
			{
				assert_children_in_range(1);
				this->index_ += 1;
				return *this;
			}

			[[nodiscard]] iterator_implementation operator++(int)
			{
				return ++iterator(*this);
			}

			iterator_implementation& operator--()
			{
				assert_children_in_range( - 1);
				this->index_ -= 1;
				return *this;
			}

			[[nodiscard]] iterator_implementation operator--(int)
			{
				return --iterator(*this);
			}

		public:
			[[nodiscard]] bool operator==(const iterator_implementation& rhs) const noexcept
			{
				if (this->node_ != rhs.node_)
					return false;

				if (this->node_ == nullptr)
					return true;

				return this->index_ == rhs.index_;
			}

			[[nodiscard]] bool operator!=(const iterator_implementation& rhs) const noexcept
			{
				return !(*this == rhs);
			}

		public:
			iterator_implementation& operator+=(difference_type n)
			{
				assert_children_in_range(n);
				this->index_ += n;
				return *this;
			}

			[[nodiscard]] iterator_implementation operator+(difference_type n) const
			{
				return (iterator_implementation(*this) += n);
			}

			[[nodiscard]] friend iterator_implementation operator+(difference_type lhs, const iterator_implementation& rhs)
			{
				return (iterator_implementation(rhs) += lhs);
			}

			iterator_implementation& operator-=(difference_type n)
			{
				assert_children_in_range(-n);
				this->index_ -= n;
				return *this;
			}

			[[nodiscard]] iterator_implementation operator-(difference_type n) const
			{
				return (iterator(*this) -= n);
			}

		public:
			difference_type operator-(const iterator_implementation& rhs) const
			{
				assert_same_domain(rhs);
				return this->index_ - rhs.index_;
			}

			[[nodiscard]] bool operator<(const iterator_implementation& rhs) const
			{
				assert_same_domain(rhs);
				return this->index_ < rhs.index_;
			}

			[[nodiscard]] bool operator>(const iterator_implementation& rhs) const
			{
				assert_same_domain(rhs);
				return this->index_ > rhs.index_;
			}

			[[nodiscard]] bool operator<=(const iterator_implementation& rhs) const
			{
				assert_same_domain(rhs);
				return this->index_ <= rhs.index_;
			}

			[[nodiscard]] bool operator>=(const iterator_implementation& rhs) const
			{
				assert_same_domain(rhs);
				return this->index_ >= rhs.index_;
			}

		public:
			[[nodiscard]] iterator_implementation begin_parent() noexcept
			{
				if (this->node_ == nullptr)
					return iterator_implementation(nullptr);

				return iterator_implementation(this->node_->parent);
			}

			[[nodiscard]] iterator_implementation end_parent() noexcept
			{
				if (this->node_ == nullptr)
					return iterator_implementation(nullptr);

				return iterator_implementation(this->node_->parent, std::in_place);
			}

			[[nodiscard]] iterator_implementation begin_child() noexcept
			{
				if (this->node_ == nullptr)
					return iterator_implementation(nullptr);

				if (this->index_ < std::ssize(this->node_->children))
					return iterator_implementation(std::addressof(this->node_->children[this->index_]));

				return iterator_implementation(nullptr);
			}

			[[nodiscard]] iterator_implementation end_child() noexcept
			{
				if (this->node_ == nullptr)
					return iterator_implementation(nullptr);

				if (this->index_ < std::ssize(this->node_->children))
					return iterator_implementation(std::addressof(this->node_->children[this->index_]), std::in_place);

				return iterator_implementation(nullptr);
			}
		};

	public:
		/**
		 * \brief contiguous_iterator to value_type
		 */
		using iterator = iterator_implementation<value_type>;

		/**
		 * \brief contiguous_iterator to const value_type
		 */
		using const_iterator = iterator_implementation<const value_type>;

		static_assert(std::random_access_iterator<iterator>);
		static_assert(std::random_access_iterator<const_iterator>);

		/**
		 * \brief Returns an iterator to the first element of the first level of the basic_stacktree.
		 * \return Iterator to the first element of the first level.
		 */
		[[nodiscard]] iterator begin() noexcept
		{
			return iterator(std::addressof(root_node_));
		}

		/**
		 * \brief Returns an iterator to the last element of the first level of the basic_stacktree.
		 * \return Iterator to the last element of the first level.
		 */
		[[nodiscard]] iterator end() noexcept
		{
			return iterator(std::addressof(root_node_), std::in_place);
		}

		/**
		 * \brief Returns an iterator to the first element of the first level of the basic_stacktree.
		 * \return Iterator to the first element of the first level.
		 */
		[[nodiscard]] const_iterator begin() const noexcept
		{
			return const_iterator(std::addressof(root_node_));
		}

		/**
		 * \brief Returns an iterator to the last element of the first level of the basic_stacktree.
		 * \return Iterator to the last element of the first level.
		 */
		[[nodiscard]] const_iterator end() const noexcept
		{
			return const_iterator(std::addressof(root_node_), std::in_place);
		}

		/**
		 * \brief Returns an iterator to the first element of the first level of the basic_stacktree.
		 * \return Iterator to the first element of the first level.
		 */
		[[nodiscard]] const_iterator cbegin() const noexcept
		{
			return const_iterator(std::addressof(root_node_));
		}

		/**
		 * \brief Returns an iterator to the last element of the first level of the basic_stacktree.
		 * \return Iterator to the last element of the first level.
		 */
		[[nodiscard]] const_iterator cend() const noexcept
		{
			return const_iterator(std::addressof(root_node_), std::in_place);
		}

	private:
		template<bool EnableFunction, class StackTraceAlloc, class UnaryFunction>
		iterator insert_internal(iterator pos, const std::basic_stacktrace<StackTraceAlloc>& stacktrace, UnaryFunction& f)
		{
			static constexpr typename iterator::difference_type return_pos_npos
				= std::numeric_limits<typename iterator::difference_type>::min();
			typename iterator::difference_type return_pos = return_pos_npos;

			node* node = pos.node_;
			if (node == nullptr)
				throw std::out_of_range("Iterator does not point to any stacktree node.");

			for(const std::stacktrace_entry& entry : stacktrace | std::views::reverse)
			{
				auto& children = node->children;

				auto first = std::lower_bound(
					std::begin(children),
					std::end(children),
					entry,
					std::less{}
				);

				if (first == std::end(children) || static_cast<const std::stacktrace_entry&>(first->stacktrace) != entry) // Insert a new entry
				{
					// Construct new entry, we need to sort the vector
					typename std::remove_cvref_t<decltype(children)>::iterator it;
					it = children.emplace(first, node, entry); // Update children's parents

					for(auto& c : children)
					{
						for(auto& cc : c.children)
						{
							cc.parent = std::addressof(c);
						}
					}

					if constexpr(EnableFunction)
						std::invoke(f, it->stacktrace);

					if (return_pos == return_pos_npos)
						return_pos = std::distance(std::begin(children), it);

					node = std::addressof(*it);
				}
				else
				{
					// Enter an already existing node and merge args
					if (return_pos == return_pos_npos)
						return_pos = std::distance(std::begin(children), first);

					// Merge values
					if constexpr (EnableFunction)
						std::invoke(f, first->stacktrace);

					node = std::addressof(*first);
				}
			}

			return iterator(pos.node_, return_pos);
		}

		template<bool EnableFunction, class InputIt, class UnaryFunction>
		iterator insert_range_internal(InputIt first, InputIt last, iterator pos, UnaryFunction& f)
		{
			iterator out = pos;
			for(; first != last; ++first)
			{
				out = insert_internal<EnableFunction>(out, *first, f);
			}

			return out;
		}

	public:
		/**
		 * \brief Inserts std::basic_stacktrace<Allocator> into basic_stacktree and invokes function f on the inserted elements.
		 * \tparam UnaryFunction Unary function compatible with T& (if T is not void) or value_type&.
		 * \tparam StackTraceAllocator std::basic_stacktrace's allocator.
		 * \param stacktrace std::basic_stacktrace to insert into basic_stacktree.
		 * \param f Unary function which will modify inserted values.
		 * \return Iterator to the lowest inserted node.
		 */
		template<class UnaryFunction, class StackTraceAllocator>
		iterator insert(const std::basic_stacktrace<StackTraceAllocator>& stacktrace, UnaryFunction f)
		requires (std::is_same_v<T, void> && std::invocable<UnaryFunction, T&>) || std::invocable<UnaryFunction, value_type&>
		{
			return insert_internal<true>(this->begin(), stacktrace, f);
		}

		/**
		 * \brief Inserts std::basic_stacktrace<Allocator> into basic_stacktree.
		 * \tparam StackTraceAllocator std::basic_stacktrace's allocator.
		 * \param stacktrace std::basic_stacktrace to insert into basic_stacktree.
		 * \return Iterator to the lowest inserted node.
		 */
		template<class StackTraceAllocator>
		iterator insert(const std::basic_stacktrace<StackTraceAllocator>& stacktrace)
		{
			auto v = nullptr;
			return insert_internal<false>(this->begin(), stacktrace, v);
		}

		/**
		 * \brief Inserts the copies of those in the range [first, last).
		 * \tparam InputIt LegacyInputIterator which is indirect convertible to T.
		 * \param first iterator to begin of the range to copy from.
		 * \param last iterator to end of the range to copy from.
		 * \return Iterator to the lowest inserted node.
		 */
		template<std::input_iterator InputIt>
		iterator insert(InputIt first, InputIt last)
			requires is_basic_stacktrace<std::iter_value_t<InputIt>>::value
		{
			auto v = nullptr;
			return insert_range_internal<false>(first, last, this->begin(), v);
		}

		/**
		 * \brief Inserts  the initializer list il.
		 * \tparam StackTraceAllocator Stacktrace's Allocator
		 * \param il initializer list to insert the elements of the container with
		 * \return Iterator to the lowest inserted node.
		 */
		template<class StackTraceAllocator>
		iterator insert(std::initializer_list<std::basic_stacktrace<StackTraceAllocator>> il)
		{
			return this->insert(std::begin(il), std::end(il));
		}

		/**
		 * \brief Inserts the copies of those in the range rg.
		 * \tparam R input_range whose elements are convertible to T
		 * \param rg a container compatible range, that is, an input_range whose elements are convertible to T
		 */
		template<std::ranges::input_range R>
		void insert_range(R&& rg)
		requires is_basic_stacktrace<std::ranges::range_value_t<R>>::value
		{
			this->insert(std::begin(rg), std::end(rg));
		}

		/**
		 * \brief Erases the specified elements from the container. Removes the element at pos.
		 * \param pos iterator to the element to remove
		 * \return Iterator following the last removed element.
		 */
		iterator erase(const_iterator pos)
		{
			// Const-cast node ptr
			// Even tho we pass non-mutable iterator, this function mutates the object
			// the underlying iterator points to, we do not violate const-correctness.
			auto p_node = const_cast<node*>(pos.node_);

			if (pos == this->end())
				return iterator(p_node, pos.index_);

			auto r = p_node->children.erase(
				std::begin(p_node->children) + pos.index_);

			return iterator(p_node, std::distance(std::begin(p_node->children), r));
		}

		/**
		 * \brief Erases the specified elements from the container. Range of elements to remove.
		 * \param first iterator to begin of the range to erase.
		 * \param last iterator to end of the range to erase.
		 * \return Iterator following the last removed element.
		 */
		iterator erase(const_iterator first, const_iterator last)
		{
			// Const-cast node ptr
			// Even tho we pass non-mutable iterator, this function mutates the object
			// the underlying iterator points to, we do not violate const-correctness.
			auto p_node = const_cast<node*>(first.node_);

			if (first == last)
				return iterator(p_node, first.index_);

			first.assert_same_domain(last); // p_node is same in first and last

			auto r = p_node->children.erase(
				std::begin(p_node->children) + first.index_, std::begin(p_node->children) + last.index_);
			
			return iterator(p_node, std::distance(std::begin(p_node->children), r));
		}

	public:
		/**
		 * \brief Checks if the container has no elements.
		 * \return true if the container is empty, false otherwise.
		 */
		[[nodiscard]] bool empty() const noexcept
		{
			return std::empty(this->root_node_.children);
		}

		/**
		 * \brief Erases all elements from the basic_stacktree.
		 */
		void clear() noexcept
		{
			this->root_node_.stacktrace = {};
			return this->root_node_.children.clear();
		}

	public:
		[[nodiscard]] bool operator==(const basic_stacktree& rhs) const noexcept
		{
			return this->root_node_ == rhs.root_node_;
		}

		[[nodiscard]] bool operator!=(const basic_stacktree& rhs) const noexcept
		{
			return !(*this == rhs);
		}

	public:
		void swap(basic_stacktree& other)
			noexcept(
				std::allocator_traits<Allocator>::propagate_on_container_swap::value ||
				std::allocator_traits<Allocator>::is_always_equal::value
				)
		{
			std::swap(this->root_node_, other.root_node_);
		}

	private:
		friend void print_node(std::ostream& os, const_iterator current, size_t depth)
		{
			for (size_t i = 0; i < depth; ++i)
				os << "    ";
			
			os << static_cast<const std::stacktrace_entry&>(*current);

			if constexpr (!std::is_same_v<T, void>)
				os << " : " << current->value();

			os << '\n';

			for (auto s = current.begin_child(), e = current.end_child();
				s != e; ++s)
			{
				print_node(os, s, depth + 1);
			}
		}
	};

	template<class T = void>
	using stacktree = basic_stacktree<T, std::allocator<T>>;

	template<class T, class Alloc>
	void swap(basic_stacktree<T, Alloc>& lhs, basic_stacktree<T, Alloc>& rhs)
		noexcept(noexcept(lhs.swap(rhs)))
	{
		lhs.swap(rhs);
	}


	template<class T, class Alloc>
	std::ostream& operator<<(std::ostream& os, const basic_stacktree<T, Alloc>& st)
	{
		return print_node(os, st.begin(), 0), os;
	}

	template<class T, class Alloc>
	[[nodiscard]] std::string to_string(const basic_stacktree<T, Alloc>& st)
	{
		std::ostringstream ss;
		ss << st;
		return ss.str();
	}

	namespace pmr
	{
		template<class T = void>
		using stacktree = basic_stacktree<T, std::pmr::polymorphic_allocator<T>>;
	}
}


#endif