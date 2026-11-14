// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, type TestRequestBuilder} from '@app/api/test/TestRequestBuilder';

export const THREADS_FEATURE_HEADER = 'X-Fluxer-Features';
export const THREADS_FEATURE = 'channel_threads';

export function threadsRequest<T = unknown>(
	harness: ApiTestHarness,
	token: string,
	options: {capable?: boolean} = {},
): TestRequestBuilder<T> {
	const builder = createBuilder<T>(harness, token);
	return options.capable === false ? builder : builder.header(THREADS_FEATURE_HEADER, THREADS_FEATURE);
}
