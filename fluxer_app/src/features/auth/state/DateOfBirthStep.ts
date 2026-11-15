// SPDX-License-Identifier: AGPL-3.0-or-later

import {observable, runInAction, when} from 'mobx';

const open = observable.box(false);

export function setDateOfBirthStepOpen(value: boolean): void {
	runInAction(() => open.set(value));
}

export function isDateOfBirthStepOpen(): boolean {
	return open.get();
}

export function whenDateOfBirthStepClosed(): Promise<void> {
	return when(() => !open.get());
}
