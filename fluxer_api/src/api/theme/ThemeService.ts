// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import {FileSizeTooLargeError} from '@fluxer/errors/src/domains/core/FileSizeTooLargeError';
import {Config} from '../Config';
import type {IStorageService} from '../infrastructure/IStorageService';

export const THEME_CSS_MAX_BYTES = 8 * 1024 * 1024;

let themeCssMaxBytesOverride: number | undefined;

export function setThemeCssMaxBytesForTesting(bytes: number | undefined): void {
	themeCssMaxBytesOverride = bytes;
}

export function resolveThemeCssMaxBytes(): number {
	if (Config.dev.testModeEnabled && themeCssMaxBytesOverride !== undefined) {
		return themeCssMaxBytesOverride;
	}
	return THEME_CSS_MAX_BYTES;
}

export class ThemeService {
	constructor(private readonly storageService: IStorageService) {}

	async createTheme(css: string): Promise<{
		id: string;
	}> {
		const cssBytes = Buffer.from(css, 'utf-8');
		if (cssBytes.length > resolveThemeCssMaxBytes()) {
			throw new FileSizeTooLargeError();
		}
		const themeId = randomBytes(8).toString('hex');
		await this.storageService.uploadObject({
			bucket: Config.s3.buckets.cdn,
			key: `themes/${themeId}.css`,
			body: cssBytes,
			contentType: 'text/css; charset=utf-8',
		});
		return {id: themeId};
	}
}
