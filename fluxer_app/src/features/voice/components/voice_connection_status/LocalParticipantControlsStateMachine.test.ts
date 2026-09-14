// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type LocalParticipantControlSignals,
	selectLocalParticipantControlsViewState,
	selectLocalParticipantMediaControlState,
} from '@app/features/voice/components/voice_connection_status/LocalParticipantControlsStateMachine';
import {describe, expect, it} from 'vitest';

function signals(overrides: Partial<LocalParticipantControlSignals> = {}): LocalParticipantControlSignals {
	return {
		isConnected: true,
		canStream: true,
		isCameraEnabled: false,
		isCameraUserCapReached: false,
		isScreenShareEnabled: false,
		...overrides,
	};
}

describe('LocalParticipantControlsStateMachine', () => {
	it('waits for the room connection before exposing media actions', () => {
		const state = selectLocalParticipantControlsViewState(
			signals({
				isConnected: false,
				isCameraEnabled: true,
				isScreenShareEnabled: true,
			}),
		);
		expect(state.camera).toMatchObject({
			value: 'waiting',
			labelKey: 'waitingForConnection',
			disabled: true,
			canOpenSettings: false,
		});
		expect(state.screenShare).toMatchObject({
			value: 'waiting',
			labelKey: 'waitingForConnection',
			disabled: true,
			clickAction: 'none',
			canOpenMenu: false,
			canPreloadPicker: false,
		});
	});

	it('blocks starting camera or screen share when stream permission is missing', () => {
		const state = selectLocalParticipantControlsViewState(signals({canStream: false}));
		expect(state.camera).toMatchObject({
			value: 'permissionBlocked',
			labelKey: 'noCameraPermission',
			disabled: true,
		});
		expect(state.screenShare).toMatchObject({
			value: 'permissionBlocked',
			labelKey: 'noScreenSharePermission',
			disabled: true,
			clickAction: 'none',
		});
	});

	it('keeps active media controllable even if stream permission is later missing', () => {
		const state = selectLocalParticipantControlsViewState(
			signals({
				canStream: false,
				isCameraEnabled: true,
				isScreenShareEnabled: true,
			}),
		);
		expect(state.camera).toMatchObject({
			value: 'active',
			labelKey: 'turnOffCamera',
			disabled: false,
			canOpenSettings: true,
		});
		expect(state.screenShare).toMatchObject({
			value: 'active',
			labelKey: 'configureOrEndScreenShare',
			disabled: false,
			clickAction: 'openMenu',
			canOpenMenu: true,
			canPreloadPicker: false,
		});
	});

	it('selects picker actions for ready connected controls', () => {
		const state = selectLocalParticipantControlsViewState(signals());
		expect(state.camera).toMatchObject({
			value: 'ready',
			labelKey: 'turnOnCamera',
			disabled: false,
			canOpenSettings: true,
		});
		expect(state.screenShare).toMatchObject({
			value: 'ready',
			labelKey: 'shareScreen',
			disabled: false,
			clickAction: 'openPicker',
			canOpenMenu: false,
			canPreloadPicker: true,
		});
	});

	it('evaluates camera and screen share independently', () => {
		const mixed = signals({
			isCameraEnabled: true,
			isScreenShareEnabled: false,
		});
		expect(selectLocalParticipantMediaControlState('camera', mixed)).toBe('active');
		expect(selectLocalParticipantMediaControlState('screenShare', mixed)).toBe('ready');
	});

	it('blocks turning the camera on at the camera user cap but leaves screen share alone', () => {
		const capped = signals({isCameraUserCapReached: true});
		const state = selectLocalParticipantControlsViewState(capped);
		expect(state.camera).toMatchObject({
			value: 'capBlocked',
			labelKey: 'cameraUserCapReached',
			disabled: true,
			canOpenSettings: false,
		});
		expect(state.screenShare).toMatchObject({value: 'ready', disabled: false});
	});

	it('never blocks turning the camera off at the camera user cap', () => {
		const capped = signals({isCameraUserCapReached: true, isCameraEnabled: true});
		expect(selectLocalParticipantMediaControlState('camera', capped)).toBe('active');
		expect(selectLocalParticipantControlsViewState(capped).camera.disabled).toBe(false);
	});
});
