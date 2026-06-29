// SPDX-License-Identifier: AGPL-3.0-or-later

import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import type {IGifProvider} from './IGifProvider';

export class GifService {
	private readonly provider: IGifProvider;

	constructor(provider: IGifProvider) {
		this.provider = provider;
	}

	getProvider(): IGifProvider {
		return this.provider;
	}

	async getActive(): Promise<IGifProvider> {
		if (!(await this.provider.isAvailable())) {
			throw new FeatureTemporarilyDisabledError();
		}
		return this.provider;
	}

	getByName(name: string): IGifProvider | null {
		return name === this.provider.meta.name ? this.provider : null;
	}
}
