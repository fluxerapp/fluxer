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

import {useId} from '@floating-ui/react';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import AccessibilityStore from '~/stores/AccessibilityStore';
import LayerManager from '~/stores/LayerManager';
import MobileLayoutStore from '~/stores/MobileLayoutStore';

export type ModalSize = 'medium' | 'small' | 'large' | 'xlarge' | 'fullscreen';
export type LabelSource = 'header' | 'screen-reader';

export interface ModalStackContextValue {
	stackIndex: number;
	isVisible: boolean;
	needsBackdrop: boolean;
}

export const ModalStackContext = React.createContext<ModalStackContextValue>({
	stackIndex: 0,
	isVisible: true,
	needsBackdrop: true,
});

export type ModalTransitionPreset = 'default' | 'instant';

export interface ModalProps {
	children: React.ReactNode;
	className?: string;
	size?: ModalSize;
	initialFocusRef?: React.RefObject<HTMLElement | null> | React.RefObject<HTMLElement>;
	centered?: boolean;
	onClose?: () => void;
	onAnimationComplete?: () => void;
	backdropSlot?: React.ReactNode;
	transitionPreset?: ModalTransitionPreset;
}

export interface ModalContextValue {
	getDefaultLabelId: (source: LabelSource) => string;
	registerLabel: (source: LabelSource, id: string) => () => void;
}

export interface ModalLogicState {
	isMobile: boolean;
	isFullscreenSize: boolean;
	isFullscreenOnMobile: boolean;
	useFullscreenLayer: boolean;
	useMobileEdgeToEdge: boolean;
	prefersReducedMotion: boolean;
	baseLabelId: string;
	modalKey: string;
	modalContextValue: ModalContextValue;
	handleBackdropClick: (onClose?: () => void) => void;
	handleClose: (onClose?: () => void) => void;
	registerLabel: (source: LabelSource, id: string) => () => void;
	getDefaultLabelId: (source: LabelSource) => string;
}

export const useModalLogic = ({
	size = 'medium',
	centered = false,
	onClose,
	onAnimationComplete: _onAnimationComplete,
}: Pick<ModalProps, 'size' | 'centered' | 'onClose' | 'onAnimationComplete'>): ModalLogicState => {
	const isMobile = MobileLayoutStore.enabled;
	const isFullscreenSize = size === 'fullscreen';
	const isFullscreenOnMobile = isMobile && !centered;
	const useFullscreenLayer = isFullscreenSize || isFullscreenOnMobile;
	const useMobileEdgeToEdge = isMobile && useFullscreenLayer;
	const prefersReducedMotion = AccessibilityStore.useReducedMotion;
	const baseLabelId = useId() || 'modal';
	const modalKey = React.useRef(Math.random().toString(36).substring(7)).current;

	const [labelRegistry, setLabelRegistry] = React.useState<Partial<Record<LabelSource, string>>>({});
	const [hasMounted, setHasMounted] = React.useState(false);
	const backdropContaminatedRef = React.useRef<boolean>(false);

	const registerLabel = React.useCallback((source: LabelSource, id: string) => {
		setLabelRegistry((current) => ({...current, [source]: id}));
		return () => {
			setLabelRegistry((current) => {
				if (current[source] !== id) {
					return current;
				}
				const next = {...current};
				delete next[source];
				return next;
			});
		};
	}, []);

	const getDefaultLabelId = React.useCallback((source: LabelSource) => `${baseLabelId}-${source}`, [baseLabelId]);

	const labelledBy = React.useMemo(() => {
		const ids = Object.values(labelRegistry).filter(Boolean);
		return ids.length > 0 ? ids.join(' ') : undefined;
	}, [labelRegistry]);

	const modalContextValue = React.useMemo(
		() => ({getDefaultLabelId, registerLabel}),
		[getDefaultLabelId, registerLabel],
	);

	React.useEffect(() => {
		if (typeof queueMicrotask === 'function') {
			queueMicrotask(() => setHasMounted(true));
			return;
		}
		Promise.resolve().then(() => setHasMounted(true));
	}, []);

	React.useEffect(() => {
		if (!hasMounted || labelledBy) {
			return;
		}
		throw new Error(
			'Modal.Root requires either a Modal.Header or Modal.ScreenReaderLabel to provide an accessible label.',
		);
	}, [hasMounted, labelledBy]);

	React.useEffect(() => {
		LayerManager.addLayer('modal', modalKey, onClose);
		return () => {
			LayerManager.removeLayer('modal', modalKey);
		};
	}, [onClose, modalKey]);

	const handleBackdropClick = React.useCallback(
		(customOnClose?: () => void) => {
			if (backdropContaminatedRef.current) {
				return;
			}
			backdropContaminatedRef.current = true;

			setTimeout(() => {
				backdropContaminatedRef.current = false;
			}, 100);

			if (customOnClose) {
				customOnClose();
			} else if (onClose) {
				onClose();
			} else {
				ModalActionCreators.pop();
			}
		},
		[onClose],
	);

	const handleClose = React.useCallback(
		(customOnClose?: () => void) => {
			if (customOnClose) {
				customOnClose();
			} else if (onClose) {
				onClose();
			} else {
				ModalActionCreators.pop();
			}
		},
		[onClose],
	);

	return {
		isMobile,
		isFullscreenSize,
		isFullscreenOnMobile,
		useFullscreenLayer,
		useMobileEdgeToEdge,
		prefersReducedMotion,
		baseLabelId,
		modalKey,
		modalContextValue,
		handleBackdropClick,
		handleClose,
		registerLabel,
		getDefaultLabelId,
	};
};

export interface HeaderProps {
	children?: React.ReactNode;
	icon?: React.ReactNode;
	title: React.ReactNode;
	variant?: 'light' | 'dark';
	hideCloseButton?: boolean;
	onClose?: () => void;
	id?: string;
}

export interface HeaderLogicState {
	headingId: string;
	handleClose: () => void;
}

export const useHeaderLogic = ({
	title: _title,
	onClose,
	id,
	modalContextValue,
}: Pick<HeaderProps, 'title' | 'onClose' | 'id'> & {
	modalContextValue: ModalContextValue;
}): HeaderLogicState => {
	const {getDefaultLabelId, registerLabel} = modalContextValue;
	const headingId = React.useMemo(() => id ?? getDefaultLabelId('header'), [getDefaultLabelId, id]);

	const useIsomorphicLayoutEffect = React.useLayoutEffect;

	useIsomorphicLayoutEffect(() => registerLabel('header', headingId), [headingId, registerLabel]);

	const handleClose = React.useCallback(() => {
		if (onClose) {
			onClose();
		} else {
			ModalActionCreators.pop();
		}
	}, [onClose]);

	return {
		headingId,
		handleClose,
	};
};

export interface ScreenReaderLabelProps {
	text: React.ReactNode;
	id?: string;
}

export interface ScreenReaderLabelLogicState {
	labelId: string;
}

export const useScreenReaderLabelLogic = ({
	text: _text,
	id,
	modalContextValue,
}: Pick<ScreenReaderLabelProps, 'text' | 'id'> & {
	modalContextValue: ModalContextValue;
}): ScreenReaderLabelLogicState => {
	const {getDefaultLabelId, registerLabel} = modalContextValue;
	const labelId = React.useMemo(() => id ?? getDefaultLabelId('screen-reader'), [getDefaultLabelId, id]);

	const useIsomorphicLayoutEffect = React.useLayoutEffect;

	useIsomorphicLayoutEffect(() => registerLabel('screen-reader', labelId), [labelId, registerLabel]);

	return {
		labelId,
	};
};
