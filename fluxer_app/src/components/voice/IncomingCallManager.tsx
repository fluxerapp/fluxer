/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {AnimatePresence} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import {createPortal} from 'react-dom';
import * as CallActionCreators from '~/actions/CallActionCreators';
import {IncomingCallUI} from '~/components/voice/IncomingCallUI';
import AppStorage from '~/lib/AppStorage';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import CallStateStore from '~/stores/CallStateStore';
import ChannelStore from '~/stores/ChannelStore';
import MockIncomingCallStore from '~/stores/MockIncomingCallStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import SoundStore from '~/stores/SoundStore';
import UserStore from '~/stores/UserStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import {
	INCOMING_CALL_OVERLAY_HEIGHT,
	INCOMING_CALL_OVERLAY_OFFSET,
	INCOMING_CALL_OVERLAY_STORAGE_KEY,
	INCOMING_CALL_OVERLAY_WIDTH,
} from './IncomingCallOverlayConstants';
import {useIncomingCallPortalRoot} from './IncomingCallPortal';

interface PopoutModel {
	channelId: string;
	initiatorUserId: string;
	mockChannel?: ChannelRecord;
	mockInitiator?: UserRecord;
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

function setsEqual(a: Set<string>, b: Set<string>) {
	if (a.size !== b.size) return false;
	for (const v of a) if (!b.has(v)) return false;
	return true;
}

export const IncomingCallManager: React.FC = observer(function IncomingCallManager() {
	const calls = CallStateStore.getActiveCalls();
	const mockCall = MockIncomingCallStore.mockCall;
	const portalRoot = useIncomingCallPortalRoot();
	const selectedChannelId = SelectedChannelStore.currentChannelId;
	const currentUserId = AuthenticationStore.currentUserId;

	const [activePopouts, setActivePopouts] = useState<Set<string>>(() => new Set());
	const [windowSize, setWindowSize] = useState<WindowSize>(() => getWindowSize());
	const [basePosition, setBasePosition] = useState<Position>(() => {
		const stored = AppStorage.getJSON<Position>(INCOMING_CALL_OVERLAY_STORAGE_KEY);
		if (stored?.x != null && stored?.y != null) return stored;
		return getCenterPosition(getWindowSize());
	});

	const isInCurrentCall = useCallback(
		(channelId: string) => MediaEngineStore.connected && MediaEngineStore.channelId === channelId,
		[],
	);

	const {nextPopoutIds, popoutModels, hasRingingCalls} = useMemo(() => {
		const nextIds = new Set<string>();
		const models: Array<PopoutModel> = [];
		let hasRinging = false;

		for (const call of calls) {
			const isRingingForCurrentUser =
				Boolean(currentUserId && CallStateStore.isUserPendingRinging(call.channelId, currentUserId)) &&
				!isInCurrentCall(call.channelId);
			if (isRingingForCurrentUser) {
				hasRinging = true;
			}

			const shouldShow = isRingingForCurrentUser && selectedChannelId !== call.channelId;
			if (!shouldShow) continue;

			const initiatorUserId = call.ringing[0];
			if (!initiatorUserId) continue;

			nextIds.add(call.channelId);
			models.push({
				channelId: call.channelId,
				initiatorUserId,
			});
		}

		if (mockCall) {
			hasRinging = true;
			nextIds.add(mockCall.channel.id);
			models.push({
				channelId: mockCall.channel.id,
				initiatorUserId: mockCall.initiator.id,
				mockChannel: mockCall.channel,
				mockInitiator: mockCall.initiator,
			});
		}

		return {
			nextPopoutIds: nextIds,
			popoutModels: models,
			hasRingingCalls: hasRinging,
		};
	}, [calls, currentUserId, isInCurrentCall, mockCall, selectedChannelId]);

	useEffect(() => {
		setActivePopouts((prev) => (setsEqual(prev, nextPopoutIds) ? prev : nextPopoutIds));
	}, [nextPopoutIds]);

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

	const wasRingingRef = useRef(false);
	useEffect(() => {
		const wasRinging = wasRingingRef.current;

		if (hasRingingCalls && !wasRinging) {
			SoundStore.startIncomingRing();
		} else if (!hasRingingCalls && wasRinging) {
			SoundStore.stopIncomingRing();
		}

		wasRingingRef.current = hasRingingCalls;
	}, [hasRingingCalls]);

	useEffect(() => () => SoundStore.stopIncomingRing(), []);

	const maxOverlayX = Math.max(0, windowSize.width - INCOMING_CALL_OVERLAY_WIDTH);
	const maxOverlayY = Math.max(0, windowSize.height - INCOMING_CALL_OVERLAY_HEIGHT);

	const clampedBasePosition = useMemo(
		() => clampPositionToWindow(basePosition, windowSize),
		[basePosition, windowSize],
	);

	const handleAccept = useCallback((channelId: string) => {
		if (MockIncomingCallStore.isMockCall(channelId)) {
			MockIncomingCallStore.clearMockCall();
			return;
		}

		CallActionCreators.joinCall(channelId);
	}, []);

	const handleReject = useCallback((channelId: string) => {
		if (MockIncomingCallStore.isMockCall(channelId)) {
			MockIncomingCallStore.clearMockCall();
			return;
		}

		CallActionCreators.rejectCall(channelId);
	}, []);

	const handleIgnore = useCallback((channelId: string) => {
		if (MockIncomingCallStore.isMockCall(channelId)) {
			MockIncomingCallStore.clearMockCall();
			return;
		}

		CallActionCreators.ignoreCall(channelId);
	}, []);

	const handleDragEnd = useCallback(
		(x: number, y: number) => {
			const clamped = clampPositionToWindow({x, y}, windowSize);
			setBasePosition(clamped);
			AppStorage.setJSON(INCOMING_CALL_OVERLAY_STORAGE_KEY, clamped);
		},
		[windowSize],
	);

	const activePopoutIds = useMemo(() => Array.from(activePopouts), [activePopouts]);

	const renderedCalls = useMemo(() => {
		const modelsById = new Map(popoutModels.map((m) => [m.channelId, m]));

		const positions = new Map<string, Position>();
		let offset = 0;

		for (const channelId of activePopoutIds) {
			const x = clampNumber(clampedBasePosition.x + offset, 0, maxOverlayX);
			const y = clampNumber(clampedBasePosition.y + offset, 0, maxOverlayY);
			positions.set(channelId, {x, y});
			offset += INCOMING_CALL_OVERLAY_OFFSET;
		}

		return activePopoutIds.map((channelId) => {
			const model = modelsById.get(channelId);
			if (!model) return null;

			const channel = model.mockChannel ?? ChannelStore.getChannel(channelId) ?? null;
			const storedInitiator =
				!model.mockInitiator && model.initiatorUserId ? (UserStore.getUser(model.initiatorUserId) ?? null) : null;
			const initiator = model.mockInitiator ?? storedInitiator;
			const position = positions.get(channelId) ?? {x: 0, y: 0};

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
				/>
			);
		});
	}, [
		activePopoutIds,
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

	return createPortal(<AnimatePresence>{renderedCalls}</AnimatePresence>, portalRoot);
});
