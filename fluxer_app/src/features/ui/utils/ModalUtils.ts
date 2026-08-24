// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import LayerManager from '@app/features/ui/state/LayerManager';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import ModalState from '@app/features/ui/state/Modal';
import {ModalStackContext, UNSTACKED_MODAL_CONTEXT} from '@app/features/ui/utils/ModalStackContext';
import {useId} from '@floating-ui/react';
import {
	type ReactNode,
	type RefObject,
	useCallback,
	useContext,
	useEffect,
	useLayoutEffect,
	useMemo,
	useRef,
	useState,
} from 'react';

interface BackdropActivationWatcher {
	watcherCount: number;
	pressTarget: EventTarget | null;
	isRepeatActivation: boolean;
	isPressTargetDetached: boolean;
	handlePress: (event: Event) => void;
	handleClickActivation: (event: Event) => void;
}

const backdropActivationWatchers = new WeakMap<Document, BackdropActivationWatcher>();

function watchBackdropActivation(ownerDocument: Document): () => void {
	let watcher = backdropActivationWatchers.get(ownerDocument);
	if (!watcher) {
		const created: BackdropActivationWatcher = {
			watcherCount: 0,
			pressTarget: null,
			isRepeatActivation: false,
			isPressTargetDetached: false,
			handlePress: (event: Event) => {
				created.pressTarget = event.target;
			},
			handleClickActivation: (event: Event) => {
				const {pressTarget} = created;
				const activationTarget = event.target;
				created.isRepeatActivation = (event as MouseEvent).detail > 1;
				created.isPressTargetDetached =
					pressTarget instanceof Node && activationTarget instanceof Node && !activationTarget.contains(pressTarget);
				created.pressTarget = null;
			},
		};
		ownerDocument.addEventListener('pointerdown', created.handlePress, true);
		ownerDocument.addEventListener('mousedown', created.handlePress, true);
		ownerDocument.addEventListener('touchstart', created.handlePress, true);
		ownerDocument.addEventListener('click', created.handleClickActivation, true);
		backdropActivationWatchers.set(ownerDocument, created);
		watcher = created;
	}
	const activeWatcher = watcher;
	activeWatcher.watcherCount += 1;
	return () => {
		activeWatcher.watcherCount -= 1;
		if (activeWatcher.watcherCount > 0) {
			return;
		}
		ownerDocument.removeEventListener('pointerdown', activeWatcher.handlePress, true);
		ownerDocument.removeEventListener('mousedown', activeWatcher.handlePress, true);
		ownerDocument.removeEventListener('touchstart', activeWatcher.handlePress, true);
		ownerDocument.removeEventListener('click', activeWatcher.handleClickActivation, true);
		backdropActivationWatchers.delete(ownerDocument);
	};
}

function isBackdropActivationCarriedOver(ownerDocument: Document): boolean {
	const watcher = backdropActivationWatchers.get(ownerDocument);
	if (!watcher) {
		return false;
	}
	return watcher.isRepeatActivation || watcher.isPressTargetDetached;
}

export type ModalSize = 'medium' | 'small' | 'large' | 'xlarge' | 'fullscreen';
export type LabelSource = 'header' | 'screen-reader';

export type ModalTransitionPreset = 'default' | 'instant' | 'quick' | 'profile-slide';

export interface ModalProps {
	children: ReactNode;
	className?: string;
	size?: ModalSize;
	initialFocusRef?: RefObject<HTMLElement | null> | RefObject<HTMLElement>;
	centered?: boolean;
	onClose?: () => void;
	onAnimationComplete?: () => void;
	backdropSlot?: ReactNode;
	transitionPreset?: ModalTransitionPreset;
	disableHistoryManagement?: boolean;
}

export interface ModalContextValue {
	getDefaultLabelId: (source: LabelSource) => string;
	registerLabel: (source: LabelSource, id: string) => () => void;
	popOwningModal: () => void;
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

export function useModalLogic({
	size = 'medium',
	centered = false,
	onClose,
	onAnimationComplete: _onAnimationComplete,
}: Pick<ModalProps, 'size' | 'centered' | 'onClose' | 'onAnimationComplete'>): ModalLogicState {
	const isMobile = MobileLayout.enabled;
	const isFullscreenSize = size === 'fullscreen';
	const isFullscreenOnMobile = isMobile && !centered;
	const useFullscreenLayer = isFullscreenSize || isFullscreenOnMobile;
	const useMobileEdgeToEdge = isMobile && useFullscreenLayer;
	const prefersReducedMotion = Accessibility.useReducedMotion;
	const baseLabelId = useId() || 'modal';
	const modalKey = useRef(Math.random().toString(36).substring(7)).current;
	const stackPlacement = useContext(ModalStackContext);
	const portalHost = usePortalHost();
	const ownerDocument = portalHost?.ownerDocument ?? document;
	const stackEntryKeyRef = useRef<string | null>(null);
	const [labelRegistry, setLabelRegistry] = useState<Partial<Record<LabelSource, string>>>({});
	const [hasMounted, setHasMounted] = useState(false);
	const registerLabel = useCallback((source: LabelSource, id: string) => {
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
	const getDefaultLabelId = useCallback((source: LabelSource) => `${baseLabelId}-${source}`, [baseLabelId]);
	const labelledBy = useMemo(() => {
		const ids = Object.values(labelRegistry).filter(Boolean);
		return ids.length > 0 ? ids.join(' ') : undefined;
	}, [labelRegistry]);
	useLayoutEffect(() => {
		if (stackPlacement === UNSTACKED_MODAL_CONTEXT) {
			return;
		}
		const resolvedKey = ModalState.getKeyAtStackIndex(stackPlacement.stackIndex, ownerDocument);
		if (resolvedKey != null) {
			stackEntryKeyRef.current = resolvedKey;
		}
	}, [ownerDocument, stackPlacement]);
	useEffect(() => watchBackdropActivation(ownerDocument), [ownerDocument]);
	const popOwningModal = useCallback(() => {
		const stackEntryKey = stackEntryKeyRef.current;
		if (stackEntryKey != null) {
			ModalCommands.popWithKey(stackEntryKey);
			return;
		}
		ModalCommands.pop();
	}, []);
	const modalContextValue = useMemo(
		() => ({getDefaultLabelId, registerLabel, popOwningModal}),
		[getDefaultLabelId, popOwningModal, registerLabel],
	);
	useEffect(() => {
		if (typeof queueMicrotask === 'function') {
			queueMicrotask(() => setHasMounted(true));
			return;
		}
		Promise.resolve().then(() => setHasMounted(true));
	}, []);
	useEffect(() => {
		if (!hasMounted || labelledBy) {
			return;
		}
		throw new Error(
			'Modal.Root requires either a Modal.Header or Modal.ScreenReaderLabel to provide an accessible label.',
		);
	}, [hasMounted, labelledBy]);
	const handleClose = useCallback(
		(customOnClose?: () => void) => {
			if (customOnClose) {
				customOnClose();
			} else if (onClose) {
				onClose();
			} else {
				popOwningModal();
			}
		},
		[onClose, popOwningModal],
	);
	useEffect(() => {
		LayerManager.addLayer('modal', modalKey, handleClose);
		return () => {
			LayerManager.removeLayer('modal', modalKey);
		};
	}, [handleClose, modalKey]);
	const handleBackdropClick = useCallback(
		(customOnClose?: () => void) => {
			if (isBackdropActivationCarriedOver(ownerDocument)) {
				return;
			}
			handleClose(customOnClose);
		},
		[handleClose, ownerDocument],
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
}

export interface HeaderProps {
	children?: ReactNode;
	icon?: ReactNode;
	title: ReactNode;
	variant?: 'light' | 'dark';
	hideCloseButton?: boolean;
	onClose?: () => void;
	id?: string;
}

export interface HeaderLogicState {
	headingId: string;
	handleClose: () => void;
}

export function useHeaderLogic({
	title: _title,
	onClose,
	id,
	modalContextValue,
}: Pick<HeaderProps, 'title' | 'onClose' | 'id'> & {
	modalContextValue: ModalContextValue;
}): HeaderLogicState {
	const {getDefaultLabelId, registerLabel, popOwningModal} = modalContextValue;
	const headingId = useMemo(() => id ?? getDefaultLabelId('header'), [getDefaultLabelId, id]);
	const useIsomorphicLayoutEffect = useLayoutEffect;
	useIsomorphicLayoutEffect(() => registerLabel('header', headingId), [headingId, registerLabel]);
	const handleClose = useCallback(() => {
		if (onClose) {
			onClose();
		} else {
			popOwningModal();
		}
	}, [onClose, popOwningModal]);
	return {
		headingId,
		handleClose,
	};
}

export interface ScreenReaderLabelProps {
	text: ReactNode;
	id?: string;
}

export interface ScreenReaderLabelLogicState {
	labelId: string;
}

export function useScreenReaderLabelLogic({
	text: _text,
	id,
	modalContextValue,
}: Pick<ScreenReaderLabelProps, 'text' | 'id'> & {
	modalContextValue: ModalContextValue;
}): ScreenReaderLabelLogicState {
	const {getDefaultLabelId, registerLabel} = modalContextValue;
	const labelId = useMemo(() => id ?? getDefaultLabelId('screen-reader'), [getDefaultLabelId, id]);
	const useIsomorphicLayoutEffect = useLayoutEffect;
	useIsomorphicLayoutEffect(() => registerLabel('screen-reader', labelId), [labelId, registerLabel]);
	return {
		labelId,
	};
}
