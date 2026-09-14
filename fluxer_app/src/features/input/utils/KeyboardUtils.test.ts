// SPDX-License-Identifier: AGPL-3.0-or-later

import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: unknown) => descriptor}));

describe('KeyboardUtils', () => {
	it('recognizes common keyboard activation key values', () => {
		expect(isKeyboardActivationKey('Enter')).toBe(true);
		expect(isKeyboardActivationKey(' ')).toBe(true);
		expect(isKeyboardActivationKey('Space')).toBe(true);
		expect(isKeyboardActivationKey('Spacebar')).toBe(true);
		expect(isKeyboardActivationKey('ArrowDown')).toBe(false);
	});
});
