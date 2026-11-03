// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MediaProxyNsfwMode} from '@app/api/infrastructure/IMediaService';
import {
	type ExternalFetchRequest,
	type ExternalFetchResponse,
	IUnfurlerService,
	type UnfurlOptions,
	type UnfurlResult,
} from '@app/api/infrastructure/IUnfurlerService';

export class NoopUnfurlerService extends IUnfurlerService {
	override async unfurlWithCachePolicy(
		_url: string,
		_nsfwMode?: MediaProxyNsfwMode,
		_options?: UnfurlOptions,
	): Promise<UnfurlResult> {
		return {embeds: [], cacheTtlSeconds: null};
	}

	override async verifyDomain(_domain: string, _token: string): Promise<boolean> {
		return false;
	}

	override async fetchExternal(_request: ExternalFetchRequest): Promise<ExternalFetchResponse> {
		throw new Error('External fetches are unavailable');
	}

	override async resolveTxt(_name: string): Promise<Array<string>> {
		return [];
	}

	override async activityPubPublicKey(): Promise<string | null> {
		return null;
	}
}
