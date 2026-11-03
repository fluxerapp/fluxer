// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	OtherPlatformTemplateAPI,
	OtherPlatformTemplateFetchResult,
} from '@fluxer/desktop_ipc/src/OtherPlatformTemplateContract';
import {DESKTOP_OTHER_PLATFORM_TEMPLATE_CHANNELS} from '@fluxer/desktop_ipc/src/OtherPlatformTemplateContract';

interface OtherPlatformTemplateInvoker {
	invoke: (channel: string, ...args: Array<unknown>) => Promise<unknown>;
}

interface OtherPlatformTemplatePreloadAPI {
	readonly otherPlatformTemplate: OtherPlatformTemplateAPI;
}

export function createOtherPlatformTemplatePreloadAPI(
	renderer: OtherPlatformTemplateInvoker,
): OtherPlatformTemplatePreloadAPI {
	return Object.freeze({
		otherPlatformTemplate: Object.freeze<OtherPlatformTemplateAPI>({
			fetch: (code: string) =>
				renderer.invoke(
					DESKTOP_OTHER_PLATFORM_TEMPLATE_CHANNELS.fetch,
					code,
				) as Promise<OtherPlatformTemplateFetchResult>,
		}),
	});
}
