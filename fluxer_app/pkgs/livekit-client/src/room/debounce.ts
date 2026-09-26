// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import CriticalTimers, {type TimerHandle} from './timers.ts';

export type Options<Result> = {
	isImmediate?: boolean;
	maxWait?: number;
	callback?: (data: Result) => void;
};

export interface DebouncedFunction<Args extends Array<any>, F extends (...args: Args) => any> {
	(this: ThisParameterType<F>, ...args: Args & Parameters<F>): Promise<ReturnType<F>>;
	cancel: (reason?: any) => void;
}

interface DebouncedPromise<FunctionReturn> {
	resolve: (result: FunctionReturn) => void;
	reject: (reason?: any) => void;
}

export function debounce<Args extends Array<any>, F extends (...args: Args) => any>(
	func: F,
	waitMilliseconds = 50,
	options: Options<ReturnType<F>> = {},
): DebouncedFunction<Args, F> {
	let timeoutId: TimerHandle | undefined;
	const isImmediate = options.isImmediate ?? false;
	const callback = options.callback;
	const maxWait = options.maxWait;
	let lastInvokeTime = Date.now();

	let promises: Array<DebouncedPromise<ReturnType<F>>> = [];

	function nextInvokeTimeout() {
		if (maxWait !== undefined) {
			const timeSinceLastInvocation = Date.now() - lastInvokeTime;

			if (timeSinceLastInvocation + waitMilliseconds >= maxWait) {
				return maxWait - timeSinceLastInvocation;
			}
		}

		return waitMilliseconds;
	}

	const debouncedFunction = function (this: ThisParameterType<F>, ...args: Parameters<F>) {
		return new Promise<ReturnType<F>>((resolve, reject) => {
			const invokeFunction = () => {
				timeoutId = undefined;
				lastInvokeTime = Date.now();
				if (!isImmediate) {
					const result = func.apply(this, args);
					callback?.(result);
					promises.forEach(({resolve}) => resolve(result));
					promises = [];
				}
			};

			const shouldCallNow = isImmediate && timeoutId === undefined;

			if (timeoutId !== undefined) {
				CriticalTimers.clearTimeout(timeoutId);
			}

			timeoutId = CriticalTimers.setTimeout(invokeFunction, nextInvokeTimeout());

			if (shouldCallNow) {
				const result = func.apply(this, args);
				callback?.(result);
				return resolve(result);
			}
			promises.push({resolve, reject});
		});
	};

	debouncedFunction.cancel = (reason?: any) => {
		if (timeoutId !== undefined) {
			CriticalTimers.clearTimeout(timeoutId);
		}
		promises.forEach(({reject}) => reject(reason));
		promises = [];
	};

	return debouncedFunction;
}
