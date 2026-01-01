/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import type {Gift} from '~/actions/GiftActionCreators';

export type GiftDurationPluralization = (durationMonths: number) => string;

export interface GiftDurationTextConfig {
	lifetime: string;
	oneYear: string;
	plural: GiftDurationPluralization;
}

export interface GiftDurationPayload {
	duration_months: number;
}

export const formatGiftDurationText = (durationMonths: number, config: GiftDurationTextConfig): string => {
	if (durationMonths === 0) {
		return config.lifetime;
	}

	if (durationMonths === 12) {
		return config.oneYear;
	}

	return config.plural(durationMonths);
};

export const getPlutoniumDurationConfig = (i18n: I18n): GiftDurationTextConfig => ({
	lifetime: i18n._(msg`Visionary (Lifetime Plutonium)`),
	oneYear: i18n._(msg`1 Year of Plutonium`),
	plural: (durationMonths: number) =>
		i18n._(
			durationMonths === 1 ? msg`${durationMonths} Month of Plutonium` : msg`${durationMonths} Months of Plutonium`,
		),
});

export const getPremiumDurationConfig = (i18n: I18n): GiftDurationTextConfig => ({
	lifetime: i18n._(msg`Lifetime Plutonium`),
	oneYear: i18n._(msg`1 Year of Plutonium`),
	plural: (durationMonths: number) =>
		i18n._(
			durationMonths === 1 ? msg`${durationMonths} Month of Plutonium` : msg`${durationMonths} Months of Plutonium`,
		),
});

export const getGiftDurationText = (i18n: I18n, gift: Gift | GiftDurationPayload): string =>
	formatGiftDurationText(gift.duration_months, getPlutoniumDurationConfig(i18n));

export const getPremiumGiftDurationText = (i18n: I18n, gift: Gift | GiftDurationPayload): string =>
	formatGiftDurationText(gift.duration_months, getPremiumDurationConfig(i18n));
