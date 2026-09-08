// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '../Config';

let cachedCollectDateOfBirth: boolean | null = null;

export function getDefaultDateOfBirthCollection(): boolean {
	return !Config.instance.selfHosted;
}

export function instanceCollectsDateOfBirth(): boolean {
	return cachedCollectDateOfBirth ?? getDefaultDateOfBirthCollection();
}

export function setCachedDateOfBirthCollection(collect: boolean): void {
	cachedCollectDateOfBirth = collect;
}
