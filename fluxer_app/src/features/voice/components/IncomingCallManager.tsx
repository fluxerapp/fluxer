// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import Sound from '@app/features/ui/state/Sound';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as CallCommands from '@app/features/voice/commands/CallCommands';
import {
	createIncomingCallManagerSnapshot,
	resolveIncomingRingCommand,
	selectIncomingCallManagerModel,
	transitionIncomingCallManagerSnapshot,
} from '@app/features/voice/components/IncomingCallManagerStateMachine';
import {
	INCOMING_CALL_OVERLAY_HEIGHT,
	INCOMING_CALL_OVERLAY_STORAGE_KEY,
	INCOMING_CALL_OVERLAY_WIDTH,
} from '@app/features/voice/components/IncomingCallOverlayConstants';
import {useIncomingCallPortalRoot} from '@app/features/voice/components/IncomingCallPortal';
import {IncomingCallUI} from '@app/features/voice/components/IncomingCallUI';
import VoiceRuntime, {useVoiceRuntimeVersion} from '@app/features/voice/engine/VoiceRuntime';
import CallInitiator from '@app/features/voice/state/CallInitiator';
import CallState from '@app/features/voice/state/CallState';
import MockIncomingCall from '@app/features/voice/state/MockIncomingCall';
import {areOrderedStringArraysEqual} from '@app/features/voice/utils/StringArrayUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {AnimatePresence} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import {createPortal} from 'react-dom';

interface PopoutModel {
	channelId: string;
	initiatorUserId: string | null;
	mockChannel?: Channel;
	mockInitiator?: User;
}

interface Position {
	x: number;
	y: number;
}

interface WindowSize {
	width: number;
	height: number;
}

function clampNumber(value: number, min: number, max: number): number {
	return Math.min(Math.max(value, min), max);
}

function getWindowSize(): WindowSize {
	return {
		width: window.innerWidth,
		height: window.innerHeight,
	};
}

function getCenterPosition(windowSize: WindowSize): Position {
	return {
		x: Math.max(0, windowSize.width / 2 - INCOMING_CALL_OVERLAY_WIDTH / 2),
		y: Math.max(0, windowSize.height / 2 - INCOMING_CALL_OVERLAY_HEIGHT / 2),
	};
}

function clampPositionToWindow(position: Position, windowSize: WindowSize): Position {
	const maxX = Math.max(0, windowSize.width - INCOMING_CALL_OVERLAY_WIDTH);
	const maxY = Math.max(0, windowSize.height - INCOMING_CALL_OVERLAY_HEIGHT);
	return {
		x: clampNumber(position.x, 0, maxX),
		y: clampNumber(position.y, 0, maxY),
	};
}

function useStableStringArray(value: Array<string>): Array<string> {
	const ref = useRef(value);
	if (!areOrderedStringArraysEqual(ref.current, value)) {
		ref.current = value;
	}
	return ref.current;
}

function resolveInitiatorUserId(
	channel: Channel | null,
	ringing: Array<string>,
	currentUserId: string | null,
): string | null {
	if (channel?.type === ChannelTypes.DM && currentUserId) {
		const otherRecipient = channel.recipientIds.find((id) => id !== currentUserId);
		return otherRecipient ?? ringing[0] ?? null;
	}
	if (ringing.length > 0) {
		const nonCurrent = currentUserId ? ringing.find((id) => id !== currentUserId) : undefined;
		return nonCurrent ?? ringing[0] ?? null;
	}
	return null;
}

export const IncomingCallManager: React.FC = observer(function IncomingCallManager() {
	useVoiceRuntimeVersion();
	const calls = CallState.getActiveCalls();
	const mockCall = MockIncomingCall.mockCall;
	const portalRoot = useIncomingCallPortalRoot();
	const currentUserId = Authentication.currentUserId;
	const [managerSnapshot, setManagerSnapshot] = useState(createIncomingCallManagerSnapshot);
	const [windowSize, setWindowSize] = useState<WindowSize>(() => getWindowSize());
	const [basePosition, setBasePosition] = useState<Position>(() => {
		const stored = AppStorage.getJSON<Position>(INCOMING_CALL_OVERLAY_STORAGE_KEY);
		if (stored?.x != null && stored?.y != null) return stored;
		return getCenterPosition(getWindowSize());
	});
	const isInCurrentCall = useCallback(
		(channelId: string) => VoiceRuntime.connected && VoiceRuntime.channelId === channelId,
		[],
	);
	const isVoiceConnected = VoiceRuntime.connected;
	const isVoiceConnecting = VoiceRuntime.connecting;
	const {incomingCallIds, popoutModels, hasRingingCalls} = useMemo(() => {
		const nextIds: Array<string> = [];
		const seenIds = new Set<string>();
		const models: Array<PopoutModel> = [];
		let hasRinging = false;
		for (const call of calls) {
			const isRingingForCurrentUser =
				Boolean(currentUserId && CallState.isUserPendingRinging(call.channelId, currentUserId)) &&
				!isInCurrentCall(call.channelId) &&
				!CallInitiator.hasInitiated(call.channelId);
			if (isRingingForCurrentUser) {
				hasRinging = true;
			}
			const shouldShow = isRingingForCurrentUser;
			if (!shouldShow) continue;
			const channel = Channels.getChannel(call.channelId) ?? null;
			const initiatorUserId = resolveInitiatorUserId(channel, call.ringing, currentUserId);
			if (seenIds.has(call.channelId)) continue;
			seenIds.add(call.channelId);
			nextIds.push(call.channelId);
			models.push({
				channelId: call.channelId,
				initiatorUserId,
			});
		}
		if (mockCall) {
			hasRinging = true;
			if (!seenIds.has(mockCall.channel.id)) {
				nextIds.push(mockCall.channel.id);
				models.push({
					channelId: mockCall.channel.id,
					initiatorUserId: mockCall.initiator.id,
					mockChannel: mockCall.channel,
					mockInitiator: mockCall.initiator,
				});
			}
		}
		return {
			incomingCallIds: nextIds,
			popoutModels: models,
			hasRingingCalls: hasRinging,
		};
	}, [calls, currentUserId, isInCurrentCall, mockCall]);
	const stableIncomingCallIds = useStableStringArray(incomingCallIds);
	useEffect(() => {
		setManagerSnapshot((snapshot) =>
			transitionIncomingCallManagerSnapshot(snapshot, {
				type: 'incomingCalls.update',
				signals: {
					incomingCallIds: stableIncomingCallIds,
					hasRingingCalls,
					isVoiceConnected,
					isVoiceConnecting,
				},
			}),
		);
	}, [hasRingingCalls, stableIncomingCallIds, isVoiceConnected, isVoiceConnecting]);
	const managerModel = selectIncomingCallManagerModel(managerSnapshot);
	useEffect(() => {
		const handleResize = () => setWindowSize(getWindowSize());
		window.addEventListener('resize', handleResize);
		return () => window.removeEventListener('resize', handleResize);
	}, []);
	useEffect(() => {
		const clamped = clampPositionToWindow(basePosition, windowSize);
		if (clamped.x !== basePosition.x || clamped.y !== basePosition.y) {
			setBasePosition(clamped);
		}
	}, [basePosition, windowSize]);
	const incomingCallActive = Sound.isIncomingCallActive();
	const incomingRingEnabled = Sound.isSoundTypeEnabled(SoundType.IncomingRing);
	useEffect(() => {
		const command = resolveIncomingRingCommand({
			shouldPlayIncomingRing: managerModel.shouldPlayIncomingRing,
			ringSoundEnabled: incomingRingEnabled,
			ringActive: incomingCallActive,
		});
		if (command === 'start') {
			Sound.startIncomingRing();
		} else if (command === 'stop') {
			Sound.stopIncomingRing();
		}
	}, [managerModel.shouldPlayIncomingRing, incomingRingEnabled, incomingCallActive]);
	useEffect(() => () => Sound.stopIncomingRing(), []);
	const maxOverlayX = Math.max(0, windowSize.width - INCOMING_CALL_OVERLAY_WIDTH);
	const maxOverlayY = Math.max(0, windowSize.height - INCOMING_CALL_OVERLAY_HEIGHT);
	const clampedBasePosition = useMemo(
		() => clampPositionToWindow(basePosition, windowSize),
		[basePosition, windowSize],
	);
	const handleAccept = useCallback((channelId: string) => {
		if (MockIncomingCall.isMockCall(channelId)) {
			MockIncomingCall.clearMockCall();
			return;
		}
		CallCommands.joinCall(channelId);
	}, []);
	const handleReject = useCallback((channelId: string) => {
		if (MockIncomingCall.isMockCall(channelId)) {
			MockIncomingCall.clearMockCall();
			return;
		}
		CallCommands.rejectCall(channelId);
	}, []);
	const handleIgnore = useCallback((channelId: string) => {
		if (MockIncomingCall.isMockCall(channelId)) {
			MockIncomingCall.clearMockCall();
			return;
		}
		CallCommands.ignoreCall(channelId);
	}, []);
	const handleDragEnd = useCallback(
		(x: number, y: number) => {
			const clamped = clampPositionToWindow({x, y}, windowSize);
			setBasePosition(clamped);
			AppStorage.setJSON(INCOMING_CALL_OVERLAY_STORAGE_KEY, clamped);
		},
		[windowSize],
	);
	const renderedCalls = useMemo(() => {
		const modelsById = new Map(popoutModels.map((m) => [m.channelId, m]));
		const activeCallId = managerModel.callQueue.find((channelId) => modelsById.has(channelId));
		if (!activeCallId) return [];
		const position = {
			x: clampNumber(clampedBasePosition.x, 0, maxOverlayX),
			y: clampNumber(clampedBasePosition.y, 0, maxOverlayY),
		};
		return [activeCallId].map((channelId) => {
			const model = modelsById.get(channelId);
			if (!model) return null;
			const channel = model.mockChannel ?? Channels.getChannel(channelId) ?? null;
			const storedInitiator =
				!model.mockInitiator && model.initiatorUserId ? (Users.getUser(model.initiatorUserId) ?? null) : null;
			const initiator = model.mockInitiator ?? storedInitiator;
			return (
				<IncomingCallUI
					key={channelId}
					channel={channel}
					initiator={initiator}
					initialX={position.x}
					initialY={position.y}
					maxX={maxOverlayX}
					maxY={maxOverlayY}
					onAccept={() => handleAccept(channelId)}
					onReject={() => handleReject(channelId)}
					onIgnore={() => handleIgnore(channelId)}
					onDragEnd={handleDragEnd}
					data-flx="voice.incoming-call-manager.rendered-calls.incoming-call-ui"
				/>
			);
		});
	}, [
		managerModel.callQueue,
		popoutModels,
		handleAccept,
		handleReject,
		handleIgnore,
		handleDragEnd,
		clampedBasePosition.x,
		clampedBasePosition.y,
		maxOverlayX,
		maxOverlayY,
	]);
	if (renderedCalls.length === 0 || !portalRoot) return null;
	return createPortal(
		<AnimatePresence data-flx="voice.incoming-call-manager.animate-presence">{renderedCalls}</AnimatePresence>,
		portalRoot,
	);
});
