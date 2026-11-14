// SPDX-License-Identifier: AGPL-3.0-or-later

import {useEffect, useEffectEvent, useState} from 'react';

interface DebouncedRequest<Input, Value> {
	input: Input;
	load: (input: Input, signal: AbortSignal) => Promise<Value>;
}

type AsyncResource<Input, Value> =
	| {status: 'pending' | 'failed'; input: Input; value: Value | undefined}
	| {status: 'ready'; input: Input; value: Value};

export function useDebouncedAsyncResource<Input, Value>(
	request: DebouncedRequest<Input, Value> | null,
	delayMs: number,
	onError: (error: unknown, input: Input) => void,
): AsyncResource<Input, Value> | null {
	const [resource, setResource] = useState<AsyncResource<Input, Value> | null>(null);
	const reportError = useEffectEvent(onError);
	useEffect(() => {
		if (request === null) {
			return;
		}
		const controller = new AbortController();
		const {input, load} = request;
		const timer = window.setTimeout(() => {
			setResource((previous) => ({status: 'pending', input, value: previous?.value}));
			load(input, controller.signal)
				.then((value) => {
					if (controller.signal.aborted) {
						return;
					}
					setResource({status: 'ready', input, value});
				})
				.catch((error: unknown) => {
					if (controller.signal.aborted) {
						return;
					}
					reportError(error, input);
					setResource((previous) => ({status: 'failed', input, value: previous?.value}));
				});
		}, delayMs);
		return () => {
			window.clearTimeout(timer);
			controller.abort();
		};
	}, [delayMs, request]);
	return resource;
}
