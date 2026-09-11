// SPDX-License-Identifier: AGPL-3.0-or-later

import ageGeos from '@fluxer/constants/src/AgeGeos.json' with {type: 'json'};
import type {GeoEntry} from './Types';

export const AGE_RESTRICTED_GEOS: ReadonlyArray<GeoEntry> = ageGeos.ageRestrictedGeos;
export const AGE_BLOCKED_GEOS: ReadonlyArray<GeoEntry> = ageGeos.ageBlockedGeos;
