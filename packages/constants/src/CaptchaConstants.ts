// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const CaptchaProviders = {
	NONE: 'none',
	ALTCHA: 'altcha',
} as const;

export type CaptchaProvider = ValueOf<typeof CaptchaProviders>;
