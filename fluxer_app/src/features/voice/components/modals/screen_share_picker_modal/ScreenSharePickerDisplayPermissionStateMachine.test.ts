// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createScreenSharePickerDisplayPermissionSnapshot,
	type ScreenSharePickerDisplayPermissionEvent,
	selectScreenSharePickerDisplayPermissionPrompt,
	transitionScreenSharePickerDisplayPermissionSnapshot,
} from '@app/features/voice/components/modals/screen_share_picker_modal/ScreenSharePickerDisplayPermissionStateMachine';
import {describe, expect, test} from 'vitest';

function promptAfter(events: ReadonlyArray<ScreenSharePickerDisplayPermissionEvent>): string {
	let snapshot = createScreenSharePickerDisplayPermissionSnapshot();
	for (const event of events) {
		snapshot = transitionScreenSharePickerDisplayPermissionSnapshot(snapshot, event);
	}
	return selectScreenSharePickerDisplayPermissionPrompt(snapshot);
}

const BLOCKED_THEN_SETTINGS: ReadonlyArray<ScreenSharePickerDisplayPermissionEvent> = [
	{type: 'permission.check'},
	{type: 'permission.result', permission: 'denied'},
	{type: 'permission.settingsOpened'},
];

describe('screen share picker display permission', () => {
	test('a grant made in System Settings unblocks the picker without a restart', () => {
		expect(promptAfter(BLOCKED_THEN_SETTINGS)).toBe('restart-required');
		expect(promptAfter([...BLOCKED_THEN_SETTINGS, {type: 'permission.result', permission: 'granted'}])).toBe('none');
		expect(
			promptAfter([
				...BLOCKED_THEN_SETTINGS,
				{type: 'permission.check'},
				{type: 'permission.result', permission: 'granted'},
			]),
		).toBe('none');
	});

	test('a permission that still reads as denied after opening settings keeps offering the relaunch', () => {
		expect(promptAfter([...BLOCKED_THEN_SETTINGS, {type: 'permission.result', permission: 'denied'}])).toBe(
			'restart-required',
		);
		expect(
			promptAfter([
				...BLOCKED_THEN_SETTINGS,
				{type: 'permission.check'},
				{type: 'permission.result', permission: 'denied'},
			]),
		).toBe('restart-required');
	});

	test('a permission that was never asked for blocks the picker until it is granted', () => {
		expect(promptAfter([{type: 'permission.check'}, {type: 'permission.result', permission: 'not-determined'}])).toBe(
			'needs-permission',
		);
	});
});
