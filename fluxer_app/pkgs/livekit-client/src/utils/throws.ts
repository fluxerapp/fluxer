// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
type Primitives = null | undefined | string | number | bigint | boolean | symbol;

export type Throws<T, E extends Error> =
	| (T & {__throws?(error: E): void})
	| Extract<T, Primitives>
	| (T extends void ? T : never);

export type ExtractErrors<T> = T extends Throws<any, infer E> ? E : never;

export type ExtractSuccess<T> = T extends Throws<infer S, any> ? S : T;

export type CombineErrors<T extends Array<any>> = T extends [infer First, ...infer Rest]
	? ExtractErrors<First> | CombineErrors<Rest>
	: never;

export type PropagatesErrors<T, AdditionalErrors extends Error = never> = Throws<
	ExtractSuccess<T>,
	ExtractErrors<T> | AdditionalErrors
>;
