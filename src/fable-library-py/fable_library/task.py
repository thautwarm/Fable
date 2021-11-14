from typing import Awaitable, TypeVar
import asyncio

T = TypeVar("T")


async def zero():
    return


async def from_result(value):
    return value


def run(value: Awaitable[T]) -> T:
    return asyncio.run(value)


def get_awaiter(value: Awaitable[T]) -> Awaitable[T]:
    return value


def get_result(value: Awaitable[T]) -> T:
    return asyncio.run(value)


__all__ = ["get_awaiter", "get_result", "from_result", "run"]
