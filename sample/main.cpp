/*
Outputs: 
ntdll!RtlUserThreadStart+0x28 : 1050376 Bytes
	KERNEL32!BaseThreadInitThunk+0x1D : 1050376 Bytes
		stacktree_demo!mainCRTStartup+0xE : 1050376 Bytes
			stacktree_demo!__scrt_common_main+0xE : 1050376 Bytes
				stacktree_demo!__scrt_common_main_seh+0x12E : 1050376 Bytes
					stacktree_demo!invoke_main+0x39 : 1050376 Bytes
						...\stacktree\sample\main.cpp(73): stacktree_demo!main+0x1C : 1800 Bytes
							...\stacktree\sample\main.cpp(59): stacktree_demo!do_something_funny+0xC7 : 1800 Bytes
						...\stacktree\sample\main.cpp(75): stacktree_demo!main+0x21 : 1048576 Bytes
							...\stacktree\sample\main.cpp(66): stacktree_demo!do_something_not_funny+0x17 : 1048576 Bytes

 */

#include <iostream>
#include <fstream>
#include <string>
#include <thread>
#include <print>

#include <fox/stacktree.hpp>

// Just a dummy
struct global_allocator
{
	struct allocation
	{
		size_t allocation_size;
		std::stacktrace st;
	};

	std::vector<allocation> allocations;

	[[nodiscard]] void* allocate(size_t size)
	{
		auto st = std::stacktrace::current(1);
		allocations.emplace_back(size, std::move(st));
		return nullptr;
	}

	void deallocate(void* ptr)
	{
		(void)ptr;
	}

	struct allocation_size
	{
		size_t size = {};
	};

	[[nodiscard]] fox::stacktree<allocation_size> get_allocation_telemetry() const
	{
		fox::stacktree<allocation_size> out;

		for(const auto& a : allocations)
		{
			out.insert(a.st, [&](auto& v) { v.value().size += a.allocation_size; });
		}

		return out;
	}
} alloc;

std::ostream& operator<<(std::ostream& os, global_allocator::allocation_size s)
{
	return os << s.size << " Bytes";
}

void do_something_funny()
{
	for(auto i : std::views::iota(0, 10))
	{
		auto ptr = alloc.allocate(i * sizeof(std::string));
		alloc.deallocate(ptr);
	}
}

void do_something_not_funny()
{
	auto ptr = alloc.allocate(1024 * 1024);
	alloc.deallocate(ptr);
}

int main()
{
	do_something_funny();
	do_something_not_funny();

	const auto tree = alloc.get_allocation_telemetry();

	std::cout << tree << '\n';

	return 0;
}