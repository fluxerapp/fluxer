// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';
import type React from 'react';

export const SHIFT_KEY_DESCRIPTOR = msg({message: 'Shift'});

export function isKeyboardActivationKey(key: string): boolean {
	return key === 'Enter' || key === ' ' || key === 'Space' || key === 'Spacebar';
}

export function handleStopPropagationForKeyboardActivation(e: React.KeyboardEvent): void {
	if (isKeyboardActivationKey(e.key)) {
		e.stopPropagation();
	}
}

export function stopPropagationOnEnterSpace(e: React.KeyboardEvent): void {
	if (isKeyboardActivationKey(e.key)) {
		e.stopPropagation();
	}
}
