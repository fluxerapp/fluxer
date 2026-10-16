// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
const SerializerSymbol = Symbol.for('lk.serializer');

export type Serializer<Input, Output> = {
	symbol: typeof SerializerSymbol;
	parse: (raw: string) => Input;
	serialize: (val: Output) => string;
};

export function isSerializer(v: unknown): v is Serializer<any, any> {
	return typeof v === 'object' && v !== null && 'symbol' in v && v.symbol === SerializerSymbol;
}

export type SerializerInput<S> = S extends Serializer<infer Input, any> ? Input : any;
export type SerializerOutput<S> = S extends Serializer<any, infer Output> ? Output : any;

function base<Input = any, Output = any>(params: Omit<Serializer<Input, Output>, 'symbol'>): Serializer<Input, Output> {
	return {...params, symbol: SerializerSymbol};
}

function json<Input = any, Output = any>(): Serializer<Input, Output> {
	return base({
		parse: (rawString: string) => JSON.parse(rawString) as Input,
		serialize: (val: unknown) => JSON.stringify(val),
	});
}

function raw() {
	return base({
		parse: (rawString: string) => rawString,
		serialize: (val: string) => val,
	});
}

function custom<Input = any, Output = any>(
	params: Omit<Serializer<Input, Output>, 'symbol'>,
): Serializer<Input, Output> {
	return base(params);
}

export const serializers = {json, raw, custom};
