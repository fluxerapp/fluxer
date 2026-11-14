// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getDateOfBirthPolicy} from '@app/api/instance/AccountsConfigCache';
import {collectsDateOfBirth} from '@fluxer/constants/src/AccountConfigConstants';

export function getLegacyDateOfBirthCollectionDefault(): boolean {
	return !Config.instance.selfHosted;
}

export function instanceCollectsDateOfBirth(): boolean {
	return collectsDateOfBirth(getDateOfBirthPolicy());
}
