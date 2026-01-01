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

import 'highlight.js/styles/github-dark.css';
import 'katex/dist/katex.min.css';

import {i18n} from '@lingui/core';
import {I18nProvider} from '@lingui/react';
import {RoomAudioRenderer, RoomContext} from '@livekit/components-react';
import {IconContext} from '@phosphor-icons/react';
import * as Sentry from '@sentry/react';
import {observer} from 'mobx-react-lite';
import React, {type ReactNode} from 'react';

import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as WindowActionCreators from '~/actions/WindowActionCreators';

import GlobalOverlays from '~/components/layout/GlobalOverlays';
import {NativeTitlebar} from '~/components/layout/NativeTitlebar';
import {NativeTrafficLightsBackdrop} from '~/components/layout/NativeTrafficLightsBackdrop';

import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import '~/components/modals/SudoVerificationModal';

import {QUICK_SWITCHER_PORTAL_ID} from '~/components/quick-switcher/QuickSwitcherConstants';
import FocusRingScope from '~/components/uikit/FocusRing/FocusRingScope';
import {SVGMasks} from '~/components/uikit/SVGMasks';
import {IncomingCallManager} from '~/components/voice/IncomingCallManager';

import {type LayoutVariant, LayoutVariantProvider} from '~/contexts/LayoutVariantContext';

import {showMyselfTypingHelper} from '~/devtools/ShowMyselfTypingHelper';

import {useActivityRecorder} from '~/hooks/useActivityRecorder';
import {useElectronScreenSharePicker} from '~/hooks/useElectronScreenSharePicker';
import {useNativePlatform} from '~/hooks/useNativePlatform';
import {useTextInputContextMenu} from '~/hooks/useTextInputContextMenu';

import CaptchaInterceptorStore from '~/lib/CaptchaInterceptor';
import FocusManager from '~/lib/FocusManager';
import KeybindManager from '~/lib/KeybindManager';
import {startReadStateCleanup} from '~/lib/ReadStateCleanup';
import {Outlet, RouterProvider} from '~/lib/router';

import {router} from '~/router';

import AccessibilityStore from '~/stores/AccessibilityStore';
import ConnectionStore from '~/stores/ConnectionStore';
import ModalStore from '~/stores/ModalStore';
import PopoutStore from '~/stores/PopoutStore';
import ReadStateStore from '~/stores/ReadStateStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import UserStore from '~/stores/UserStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';

import {ensureAutostartDefaultEnabled} from '~/utils/AutostartUtils';
import {startDeepLinkHandling} from '~/utils/DeepLinkUtils';
import {attachExternalLinkInterceptor, getElectronAPI, getNativePlatform} from '~/utils/NativeUtils';

import styles from './App.module.css';
import Config from './Config';

interface AppWrapperProps {
	children: ReactNode;
}

export const AppWrapper = observer(({children}: AppWrapperProps) => {
	const saturationFactor = AccessibilityStore.saturationFactor;
	const alwaysUnderlineLinks = AccessibilityStore.alwaysUnderlineLinks;
	const enableTextSelection = AccessibilityStore.textSelectionEnabled;
	const fontSize = AccessibilityStore.fontSize;
	const messageGutter = AccessibilityStore.messageGutter;
	const messageGroupSpacing = AccessibilityStore.messageGroupSpacingValue;
	const reducedMotion = AccessibilityStore.useReducedMotion;
	const {platform, isNative, isMacOS} = useNativePlatform();
	useElectronScreenSharePicker();
	const syncThemeAcrossDevices = AccessibilityStore.syncThemeAcrossDevices;
	const localThemeOverride = AccessibilityStore.localThemeOverride;
	const customThemeCss = AccessibilityStore.customThemeCss;
	const [layoutVariant, setLayoutVariant] = React.useState<LayoutVariant>('app');
	const layoutVariantContextValue = React.useMemo(
		() => ({variant: layoutVariant, setVariant: setLayoutVariant}),
		[layoutVariant],
	);

	const popouts = PopoutStore.getPopouts();
	const topPopout = popouts.length ? popouts[popouts.length - 1] : null;
	const topPopoutRequiresBackdrop = Boolean(topPopout && !topPopout.disableBackdrop);

	const userSettings = UserSettingsStore;
	const room = MediaEngineStore.room;
	const ringsContainerRef = React.useRef<HTMLDivElement>(null);
	const overlayScopeRef = React.useRef<HTMLDivElement>(null);

	const recordActivity = useActivityRecorder();
	const handleUserActivity = React.useCallback(() => recordActivity(), [recordActivity]);
	const handleImmediateActivity = React.useCallback(() => recordActivity(true), [recordActivity]);
	const handleResize = React.useCallback(() => WindowActionCreators.resized(), []);
	useTextInputContextMenu();

	const effectiveTheme = React.useMemo(() => {
		return UserSettingsStore.getTheme();
	}, [userSettings.theme, syncThemeAcrossDevices, localThemeOverride]);

	const hasBlockingModal = ModalStore.hasModalOpen();

	React.useEffect(() => {
		const node = ringsContainerRef.current;
		if (!node) return;

		const shouldBlockBackground = hasBlockingModal || topPopoutRequiresBackdrop;

		node.toggleAttribute('inert', shouldBlockBackground);

		return () => {
			node.removeAttribute('inert');
		};
	}, [hasBlockingModal, topPopoutRequiresBackdrop]);

	React.useEffect(() => {
		showMyselfTypingHelper.start();
		return () => showMyselfTypingHelper.stop();
	}, []);

	React.useEffect(() => {
		startReadStateCleanup();
	}, []);

	React.useEffect(() => {
		if (!('serviceWorker' in navigator)) {
			return;
		}

		const postBadgeUpdate = (count: number) => {
			const controller = navigator.serviceWorker.controller;
			if (!controller) {
				return;
			}
			try {
				controller.postMessage({type: 'APP_UPDATE_BADGE', count});
			} catch (error) {
				console.warn('[Badge] Failed to post badge update to service worker', error);
			}
		};

		const updateBadgeFromReadState = () => {
			const channelIds = ReadStateStore.getChannelIds();
			const totalMentions = channelIds.reduce((sum, channelId) => sum + ReadStateStore.getMentionCount(channelId), 0);
			postBadgeUpdate(totalMentions);
		};

		const unsubscribe = ReadStateStore.subscribe(() => {
			updateBadgeFromReadState();
		});

		return () => {
			unsubscribe();
		};
	}, []);

	React.useEffect(() => {
		void KeybindManager.init(i18n);
		void CaptchaInterceptorStore;
		return () => {
			KeybindManager.destroy();
		};
	}, []);

	React.useEffect(() => {
		void AccessibilityStore.applyStoredZoom();

		const electronApi = getElectronAPI();
		if (!electronApi) return;

		const unsubZoomIn = electronApi.onZoomIn?.(() => void AccessibilityStore.adjustZoom(0.1));
		const unsubZoomOut = electronApi.onZoomOut?.(() => void AccessibilityStore.adjustZoom(-0.1));
		const unsubZoomReset = electronApi.onZoomReset?.(() => AccessibilityStore.updateSettings({zoomLevel: 1.0}));
		const unsubOpenSettings = electronApi.onOpenSettings?.(() => {
			ModalActionCreators.push(ModalActionCreators.modal(() => <UserSettingsModal />));
		});

		return () => {
			unsubZoomIn?.();
			unsubZoomOut?.();
			unsubZoomReset?.();
			unsubOpenSettings?.();
		};
	}, []);

	React.useEffect(() => {
		const root = document.documentElement;
		root.classList.toggle('reduced-motion', reducedMotion);
		return () => {
			root.classList.remove('reduced-motion');
		};
	}, [reducedMotion]);

	React.useEffect(() => {
		if (Config.PUBLIC_BUILD_SHA && Config.PUBLIC_BUILD_TIMESTAMP) {
			const buildInfo = Config.PUBLIC_BUILD_NUMBER
				? `build ${Config.PUBLIC_BUILD_NUMBER} (${Config.PUBLIC_BUILD_SHA})`
				: Config.PUBLIC_BUILD_SHA;
			console.info(`[BUILD INFO] ${Config.PUBLIC_PROJECT_ENV} - ${buildInfo} - ${Config.PUBLIC_BUILD_TIMESTAMP}`);
		}

		FocusManager.init();

		const shouldRegisterWindowListeners = !isNative;
		if (shouldRegisterWindowListeners && document.hasFocus()) {
			document.documentElement.classList.add('window-focused');
		}

		const preventScroll = (event: Event) => event.preventDefault();
		const handleBlur = () => {
			WindowActionCreators.focus(false);
			if (shouldRegisterWindowListeners) {
				document.documentElement.classList.remove('window-focused');
			}
		};
		const handleFocus = () => {
			WindowActionCreators.focus(true);
			if (shouldRegisterWindowListeners) {
				document.documentElement.classList.add('window-focused');
			}
			handleImmediateActivity();
		};
		const handleVisibilityChange = () => {
			WindowActionCreators.visibilityChanged(!document.hidden);
		};

		const preventPinchZoom = (event: TouchEvent) => {
			if (event.touches.length > 1) {
				event.preventDefault();
			}
		};

		if (shouldRegisterWindowListeners) {
			document.addEventListener('scroll', preventScroll);
			window.addEventListener('blur', handleBlur);
			window.addEventListener('focus', handleFocus);
			document.addEventListener('visibilitychange', handleVisibilityChange);
			window.addEventListener('mousedown', handleImmediateActivity);
			window.addEventListener('mousemove', handleUserActivity);
			window.addEventListener('keydown', handleUserActivity);
			window.addEventListener('resize', handleResize);
			window.addEventListener('touchstart', handleImmediateActivity);
			document.addEventListener('touchstart', preventPinchZoom, {passive: false});
			document.addEventListener('touchmove', preventPinchZoom, {passive: false});
		}

		return () => {
			FocusManager.destroy();
			if (shouldRegisterWindowListeners) {
				document.removeEventListener('scroll', preventScroll);
				window.removeEventListener('blur', handleBlur);
				window.removeEventListener('focus', handleFocus);
				document.removeEventListener('visibilitychange', handleVisibilityChange);
				window.removeEventListener('mousedown', handleImmediateActivity);
				window.removeEventListener('mousemove', handleUserActivity);
				window.removeEventListener('keydown', handleUserActivity);
				window.removeEventListener('resize', handleResize);
				window.removeEventListener('touchstart', handleImmediateActivity);
				document.removeEventListener('touchstart', preventPinchZoom);
				document.removeEventListener('touchmove', preventPinchZoom);
			}
		};
	}, [handleImmediateActivity, handleUserActivity, handleResize, isNative]);

	React.useEffect(() => {
		if (!isNative) {
			return;
		}
		const htmlNode = document.documentElement;
		const updateClass = (focused: boolean) => {
			htmlNode.classList.toggle('window-focused', focused);
		};
		const handleFocus = () => {
			updateClass(true);
			WindowActionCreators.focus(true);
			handleImmediateActivity();
		};
		const handleBlur = () => {
			updateClass(false);
			WindowActionCreators.focus(false);
		};
		const handleVisibilityChange = () => {
			WindowActionCreators.visibilityChanged(!document.hidden);
		};
		const preventPinchZoom = (event: TouchEvent) => {
			if (event.touches.length > 1) {
				event.preventDefault();
			}
		};
		updateClass(document.hasFocus());
		window.addEventListener('focus', handleFocus);
		window.addEventListener('blur', handleBlur);
		document.addEventListener('visibilitychange', handleVisibilityChange);
		window.addEventListener('mousedown', handleImmediateActivity);
		window.addEventListener('mousemove', handleUserActivity);
		window.addEventListener('keydown', handleUserActivity);
		window.addEventListener('resize', handleResize);
		window.addEventListener('touchstart', handleImmediateActivity);
		document.addEventListener('touchstart', preventPinchZoom, {passive: false});
		document.addEventListener('touchmove', preventPinchZoom, {passive: false});
		return () => {
			window.removeEventListener('focus', handleFocus);
			window.removeEventListener('blur', handleBlur);
			document.removeEventListener('visibilitychange', handleVisibilityChange);
			window.removeEventListener('mousedown', handleImmediateActivity);
			window.removeEventListener('mousemove', handleUserActivity);
			window.removeEventListener('keydown', handleUserActivity);
			window.removeEventListener('resize', handleResize);
			window.removeEventListener('touchstart', handleImmediateActivity);
			document.removeEventListener('touchstart', preventPinchZoom);
			document.removeEventListener('touchmove', preventPinchZoom);
		};
	}, [handleImmediateActivity, handleResize, handleUserActivity, isNative]);

	React.useEffect(() => {
		const htmlNode = document.documentElement;
		const platformClasses = [isNative ? 'platform-native' : 'platform-web', `platform-${platform}`];

		htmlNode.classList.add(...platformClasses);

		return () => {
			htmlNode.classList.remove(...platformClasses);
		};
	}, [isNative, platform]);

	React.useEffect(() => {
		if (isNative) {
			return;
		}

		const handlePageUnload = () => {
			const guildId = MediaEngineStore.guildId;
			const connected = MediaEngineStore.connected;
			const room = MediaEngineStore.room;
			const socket = ConnectionStore.socket;

			if (socket && connected && guildId) {
				try {
					if (room) {
						room.disconnect(true);
					}

					socket.updateVoiceState({
						guild_id: guildId,
						channel_id: null,
						self_mute: true,
						self_deaf: true,
						self_video: false,
						self_stream: false,
						connection_id: MediaEngineStore.connectionId ?? null,
					});
				} catch (error) {
					console.error('Failed to send disconnect on page unload:', error);
				}
			}
		};

		window.addEventListener('beforeunload', handlePageUnload);
		window.addEventListener('pagehide', handlePageUnload);

		return () => {
			window.removeEventListener('beforeunload', handlePageUnload);
			window.removeEventListener('pagehide', handlePageUnload);
		};
	}, [isNative]);

	React.useEffect(() => {
		const htmlNode = document.documentElement;
		htmlNode.classList.add(`theme-${effectiveTheme}`);
		htmlNode.style.setProperty('--saturation-factor', saturationFactor.toString());
		htmlNode.style.setProperty('--user-select', enableTextSelection ? 'auto' : 'none');
		htmlNode.style.setProperty('--font-size', `${fontSize}px`);
		htmlNode.style.setProperty('--chat-horizontal-padding', `${messageGutter}px`);
		htmlNode.style.setProperty('--message-group-spacing', `${messageGroupSpacing}px`);

		if (alwaysUnderlineLinks) {
			htmlNode.style.setProperty('--link-decoration', 'underline');
		} else {
			htmlNode.style.removeProperty('--link-decoration');
		}

		return () => {
			htmlNode.classList.remove(`theme-${effectiveTheme}`);
			htmlNode.style.removeProperty('--saturation-factor');
			htmlNode.style.removeProperty('--link-decoration');
			htmlNode.style.removeProperty('--user-select');
			htmlNode.style.removeProperty('--font-size');
			htmlNode.style.removeProperty('--chat-horizontal-padding');
			htmlNode.style.removeProperty('--message-group-spacing');
		};
	}, [
		effectiveTheme,
		saturationFactor,
		alwaysUnderlineLinks,
		enableTextSelection,
		fontSize,
		messageGutter,
		messageGroupSpacing,
	]);

	React.useEffect(() => {
		const styleElementId = 'fluxer-custom-theme-style';
		const existing = document.getElementById(styleElementId) as HTMLStyleElement | null;

		const css = customThemeCss?.trim() ?? '';

		if (!css) {
			if (existing?.parentNode) {
				existing.parentNode.removeChild(existing);
			}
			return;
		}

		const styleElement = existing ?? document.createElement('style');
		styleElement.id = styleElementId;
		styleElement.textContent = css;

		if (!existing) {
			document.head.appendChild(styleElement);
		}
	}, [customThemeCss]);

	return (
		<LayoutVariantProvider value={layoutVariantContextValue}>
			<SVGMasks />
			<RoomContext.Provider value={room ?? undefined}>
				{room && <RoomAudioRenderer />}
				<div ref={ringsContainerRef} className={styles.appContainer}>
					<FocusRingScope containerRef={ringsContainerRef}>
						<NativeTrafficLightsBackdrop variant={layoutVariant} />
						{isNative && !isMacOS && <NativeTitlebar platform={platform} />}
						{children}
					</FocusRingScope>
				</div>
				<div ref={overlayScopeRef} className={styles.overlayScope}>
					<div
						id={QUICK_SWITCHER_PORTAL_ID}
						className={styles.quickSwitcherPortal}
						data-overlay-pass-through="true"
						aria-hidden="true"
					/>
					<GlobalOverlays />
					<IncomingCallManager />
				</div>
			</RoomContext.Provider>
		</LayoutVariantProvider>
	);
});

export const App = observer((): React.ReactElement => {
	const currentUser = UserStore.currentUser;

	React.useEffect(() => {
		const initAutostart = async () => {
			const platform = await getNativePlatform();
			if (platform === 'macos') {
				void ensureAutostartDefaultEnabled();
			}
		};

		void initAutostart();
	}, []);

	React.useEffect(() => {
		void startDeepLinkHandling();
	}, []);

	React.useEffect(() => {
		const detach = attachExternalLinkInterceptor();
		return () => detach?.();
	}, []);

	React.useEffect(() => {
		if (currentUser) {
			Sentry.setUser({
				id: currentUser.id,
				username: currentUser.username,
				email: currentUser.email ?? undefined,
			});
		} else {
			Sentry.setUser(null);
		}
	}, [currentUser]);

	return (
		<I18nProvider i18n={i18n}>
			<IconContext.Provider value={{color: 'currentColor', weight: 'fill'}}>
				<RouterProvider router={router}>
					<AppWrapper>
						<Outlet />
					</AppWrapper>
				</RouterProvider>
			</IconContext.Provider>
		</I18nProvider>
	);
});
