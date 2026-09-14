// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	COMFY_MESSAGE_GROUP_SPACING_DEFAULT,
	COMPACT_MESSAGE_GROUP_SPACING_DEFAULT,
	getDefaultMessageGroupSpacing,
	getMessageGroupSpacingForDisplayMode,
	getMessageGroupSpacingPatch,
	migrateLegacyMessageGroupSpacing,
} from '@app/features/accessibility/state/MessageGroupSpacing';
import {describe, expect, it} from 'vitest';

describe('MessageGroupSpacing', () => {
	it('keeps explicit defaults per display mode', () => {
		expect(getDefaultMessageGroupSpacing(false)).toBe(COMFY_MESSAGE_GROUP_SPACING_DEFAULT);
		expect(getDefaultMessageGroupSpacing(true)).toBe(COMPACT_MESSAGE_GROUP_SPACING_DEFAULT);
	});
	it('reads the spacing for the active display mode', () => {
		const settings = {messageGroupSpacing: 16, compactMessageGroupSpacing: 0};
		expect(getMessageGroupSpacingForDisplayMode(settings, false)).toBe(16);
		expect(getMessageGroupSpacingForDisplayMode(settings, true)).toBe(0);
	});
	it('updates only the active display mode', () => {
		expect(getMessageGroupSpacingPatch(false, 8)).toEqual({messageGroupSpacing: 8});
		expect(getMessageGroupSpacingPatch(true, 8)).toEqual({compactMessageGroupSpacing: 8});
	});
	it('migrates legacy default spacing to separate mode defaults', () => {
		expect(migrateLegacyMessageGroupSpacing(16, false)).toEqual({
			messageGroupSpacing: 16,
			compactMessageGroupSpacing: 0,
		});
		expect(migrateLegacyMessageGroupSpacing(0, true)).toEqual({
			messageGroupSpacing: 16,
			compactMessageGroupSpacing: 0,
		});
	});
	it('migrates legacy custom spacing as a shared custom value', () => {
		expect(migrateLegacyMessageGroupSpacing(8, false)).toEqual({
			messageGroupSpacing: 8,
			compactMessageGroupSpacing: 8,
		});
		expect(migrateLegacyMessageGroupSpacing(8, true)).toEqual({
			messageGroupSpacing: 8,
			compactMessageGroupSpacing: 8,
		});
	});
});
