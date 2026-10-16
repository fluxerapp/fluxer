// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	registerMessageHoverTarget,
	resolveMessageHoverTargetsNow,
} from '@app/features/channel/components/MessageHoverTracking';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

interface UseMessageHoverStateParams {
	messageRef: React.RefObject<HTMLDivElement | null>;
	mobileLayoutEnabled: boolean;
	contextMenuOpen: boolean;
}

export interface MessageHoverState {
	isHovering: boolean;
	isPopoutOpen: boolean;
	handlePopoutToggle: (isOpen: boolean) => void;
}

export function useMessageHoverState({
	messageRef,
	mobileLayoutEnabled,
	contextMenuOpen,
}: UseMessageHoverStateParams): MessageHoverState {
	const [isHoveringDesktop, setIsHoveringDesktop] = useState(false);
	const [isPopoutOpen, setIsPopoutOpen] = useState(false);
	const isHoveringDesktopRef = useRef(false);
	const setDesktopHoverState = useCallback((isHovered: boolean) => {
		if (isHoveringDesktopRef.current === isHovered) {
			return;
		}
		isHoveringDesktopRef.current = isHovered;
		setIsHoveringDesktop(isHovered);
	}, []);
	const handlePopoutToggle = useCallback((isOpen: boolean) => {
		setIsPopoutOpen(isOpen);
	}, []);
	useEffect(() => {
		if (mobileLayoutEnabled) {
			return;
		}
		const element = messageRef.current;
		if (element == null) {
			return;
		}
		return registerMessageHoverTarget(element, setDesktopHoverState);
	}, [mobileLayoutEnabled, messageRef, setDesktopHoverState]);
	const isOverlayOpen = contextMenuOpen || isPopoutOpen;
	const wasOverlayOpenRef = useRef(false);
	useLayoutEffect(() => {
		const wasOpen = wasOverlayOpenRef.current;
		wasOverlayOpenRef.current = isOverlayOpen;
		if (!wasOpen || isOverlayOpen) {
			return;
		}
		resolveMessageHoverTargetsNow();
	}, [isOverlayOpen]);
	return {
		isHovering: mobileLayoutEnabled ? false : isHoveringDesktop,
		isPopoutOpen,
		handlePopoutToggle,
	};
}
