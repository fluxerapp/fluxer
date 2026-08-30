// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';

const errors: Array<unknown> = [];

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		error(...args: Array<unknown>) {
			errors.push(args);
		}
	},
}));

const {normalizeVoiceParticipantMenuRequest} = await import(
	'@app/features/ui/action_menu/items/VoiceParticipantMenuTypes'
);

describe('normalizeVoiceParticipantMenuRequest', () => {
	it('passes participant sources through on every surface', () => {
		expect(normalizeVoiceParticipantMenuRequest({surface: 'participant-list', source: {kind: 'participant'}})).toEqual({
			kind: 'participant',
		});
		expect(
			normalizeVoiceParticipantMenuRequest({
				surface: 'stream-spectator-list',
				source: {kind: 'participant', focusSource: 'camera'},
			}),
		).toEqual({kind: 'participant', focusSource: 'camera'});
	});

	it('keeps camera and screen-share sources on the call-tile surface', () => {
		expect(normalizeVoiceParticipantMenuRequest({surface: 'call-tile', source: {kind: 'camera'}})).toEqual({
			kind: 'camera',
		});
		const screenShare = {kind: 'screen-share', streamKey: 'guild:channel:conn', state: {kind: 'own'}} as const;
		expect(normalizeVoiceParticipantMenuRequest({surface: 'call-tile', source: screenShare})).toBe(screenShare);
	});

	it('degrades to a participant menu instead of throwing on a mismatched surface', () => {
		errors.length = 0;
		expect(normalizeVoiceParticipantMenuRequest({surface: 'call-avatar', source: {kind: 'camera'}})).toEqual({
			kind: 'participant',
			focusSource: 'camera',
		});
		expect(
			normalizeVoiceParticipantMenuRequest({
				surface: 'participant-list',
				source: {kind: 'screen-share', streamKey: 'guild:channel:conn', state: {kind: 'own'}},
			}),
		).toEqual({kind: 'participant', focusSource: 'screen-share'});
		expect(errors).toHaveLength(2);
	});

	it('degrades to a participant menu when a screen-share source has an empty stream key', () => {
		errors.length = 0;
		expect(
			normalizeVoiceParticipantMenuRequest({
				surface: 'call-tile',
				source: {kind: 'screen-share', streamKey: '', state: {kind: 'own'}},
			}),
		).toEqual({kind: 'participant', focusSource: 'screen-share'});
		expect(errors).toHaveLength(1);
	});
});
