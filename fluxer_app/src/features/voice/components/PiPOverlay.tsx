// SPDX-License-Identifier: AGPL-3.0-or-later

import PiP from '@app/features/ui/state/PiP';
import {PiPOverlayInner} from '@app/features/voice/components/pip_overlay/PiPOverlayInner';
import {pipOverlayLogger} from '@app/features/voice/components/pip_overlay/shared';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import type {Room} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';
import {createPortal} from 'react-dom';

export const PiPOverlay = observer(function PiPOverlay() {
	useMediaEngineVersion();
	const isOpen = PiP.getIsOpen();
	const content = PiP.getContent();
	const room = MediaEngine.room;
	const [activeRoom, setActiveRoom] = useState<Room | null>(room);
	useEffect(() => {
		if (room) setActiveRoom(room);
	}, [room]);
	const renderRoom = activeRoom;
	const portalRoot = typeof document === 'undefined' ? null : document.body;
	const blockReason = !portalRoot
		? 'missing-portal-root'
		: !isOpen
			? 'pip-closed'
			: !content
				? 'missing-content'
				: null;
	useEffect(() => {
		pipOverlayLogger.debug('PiP overlay shell state', {
			blockReason,
			rendererMode: renderRoom ? 'livekit' : 'native-or-placeholder',
			isOpen,
			content,
			hasCurrentRoom: Boolean(room),
			currentRoomLocalParticipantIdentity: room?.localParticipant.identity ?? null,
			hasActiveRoom: Boolean(activeRoom),
			activeRoomLocalParticipantIdentity: activeRoom?.localParticipant.identity ?? null,
			renderRoomLocalParticipantIdentity: renderRoom?.localParticipant.identity ?? null,
			hasPortalRoot: Boolean(portalRoot),
		});
	}, [activeRoom, blockReason, content, isOpen, portalRoot, renderRoom, room]);
	if (!portalRoot || !isOpen || !content) return null;
	return createPortal(
		<PiPOverlayInner
			key="pip-overlay"
			content={content}
			room={renderRoom}
			data-flx="voice.pi-p-overlay.pi-p-overlay-inner"
		/>,
		portalRoot,
	);
});
