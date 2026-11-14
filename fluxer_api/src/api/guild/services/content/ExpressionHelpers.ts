// SPDX-License-Identifier: AGPL-3.0-or-later

import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {getErrorMessageUnsafe} from '@fluxer/errors/src/i18n/ErrorI18n';

export function resolveGuildLimit(
	limitConfigService: LimitConfigService,
	key: LimitKey,
	fallback: number,
	guildFeatures: Iterable<string> | null,
): number {
	const context = createLimitMatchContext({user: null, guildFeatures});
	return Math.floor(resolveLimitSafe(limitConfigService.getConfigSnapshot(), context, key, fallback, 'guild'));
}

export function getLocalizedBulkCreateError(error: unknown, locale: string | null | undefined): string {
	if (error instanceof FluxerError) {
		return getErrorMessageUnsafe(error.code, locale, error.messageVariables, error.message);
	}
	return getContentMessage('guild.bulk_create.unknown_error', locale);
}
