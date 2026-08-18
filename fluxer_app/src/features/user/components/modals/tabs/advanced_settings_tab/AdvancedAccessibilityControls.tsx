// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {
	getCachedDesktopWindowBehaviorSettings,
	getDesktopWindowBehaviorSettings,
	relaunchDesktopApp,
	setDesktopWindowBehaviorSettings,
} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import type {DesktopWindowBehaviorSettings} from '@app/types/electron.d';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useLayoutEffect, useState} from 'react';

const ENABLE_TEXT_SELECTION_DESCRIPTOR = msg({
	message: 'Enable text selection',
	comment: 'Short label for an advanced app interaction toggle.',
});
const SHOW_VIDEO_SEEK_PREVIEW_THUMBNAILS_DESCRIPTOR = msg({
	message: 'Enable video seek thumbnails',
	comment: 'Short label for the advanced video seek preview toggle.',
});
const SMOOTH_SCROLLING_DESCRIPTOR = msg({
	message: 'Smooth scrolling',
	comment: 'Short label for an advanced desktop scrolling toggle.',
});
const STAY_FULLY_INTERACTIVE_WHEN_UNFOCUSED_DESCRIPTOR = msg({
	message: 'Stay fully interactive when unfocused',
	comment: 'Short label for an advanced app interaction toggle.',
});
const USE_AUTOSCROLLING_DESCRIPTOR = msg({
	message: 'Use autoscrolling',
	comment: 'Short label for an advanced desktop scrolling toggle.',
});
const RESTART_NOW_DESCRIPTOR = msg({
	message: 'Restart now',
	comment: 'Short confirmation button label in advanced settings.',
});
const RESTART_PRODUCT_DESCRIPTOR = msg({
	message: 'Restart {productName}?',
	comment: 'Confirmation prompt in advanced settings. Preserve {productName}; it is inserted by code.',
});
const LATER_DESCRIPTOR = msg({
	message: 'Later',
	comment: 'Short confirmation button label in advanced settings.',
});

function hasDesktopSmoothScrollingPendingRestart(settings: DesktopWindowBehaviorSettings | null): boolean {
	return settings !== null && settings.smoothScrolling !== settings.activeSmoothScrolling;
}

function hasDesktopMiddleClickAutoscrollPendingRestart(settings: DesktopWindowBehaviorSettings | null): boolean {
	return settings !== null && settings.middleClickAutoscroll !== settings.activeMiddleClickAutoscroll;
}

function useDesktopWindowBehaviorSettings() {
	const cachedDesktopWindowBehavior = getCachedDesktopWindowBehaviorSettings();
	const [desktopWindowBehavior, setDesktopWindowBehavior] = useState<DesktopWindowBehaviorSettings | null>(
		cachedDesktopWindowBehavior,
	);
	const [desktopWindowBehaviorBusy, setDesktopWindowBehaviorBusy] = useState(cachedDesktopWindowBehavior === null);
	useLayoutEffect(() => {
		let mounted = true;
		if (getCachedDesktopWindowBehaviorSettings() !== null) {
			setDesktopWindowBehaviorBusy(false);
			return () => {
				mounted = false;
			};
		}
		const initDesktopWindowBehavior = async () => {
			const settings = await getDesktopWindowBehaviorSettings();
			if (!mounted) return;
			if (settings !== null) {
				setDesktopWindowBehavior(settings);
			}
			setDesktopWindowBehaviorBusy(false);
		};
		void initDesktopWindowBehavior();
		return () => {
			mounted = false;
		};
	}, []);
	const updateDesktopWindowBehavior = useCallback(async (settings: Partial<DesktopWindowBehaviorSettings>) => {
		setDesktopWindowBehaviorBusy(true);
		const nextSettings = await setDesktopWindowBehaviorSettings(settings);
		if (nextSettings !== null) {
			setDesktopWindowBehavior(nextSettings);
		}
		setDesktopWindowBehaviorBusy(false);
		return nextSettings;
	}, []);
	return {desktopWindowBehavior, desktopWindowBehaviorBusy, updateDesktopWindowBehavior};
}

function useChromiumScrollingRestartModal() {
	const {i18n} = useLingui();
	return useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(RESTART_PRODUCT_DESCRIPTOR, {productName: PRODUCT_NAME})}
					description={
						<Trans>
							{PRODUCT_NAME} needs to restart before Chromium's native scrolling settings change how scrolling works.
						</Trans>
					}
					primaryText={i18n._(RESTART_NOW_DESCRIPTOR)}
					primaryVariant="primary"
					secondaryText={i18n._(LATER_DESCRIPTOR)}
					onPrimary={async () => {
						await relaunchDesktopApp();
					}}
					data-flx="user.advanced-settings-tab.chromium-scrolling-restart.confirm-modal"
				/>
			)),
		);
	}, [i18n]);
}

export const TextSelectionControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(ENABLE_TEXT_SELECTION_DESCRIPTOR)}
			value={Accessibility.enableTextSelection}
			disabled={MobileLayout.enabled}
			onChange={(value) => AccessibilityCommands.update({enableTextSelection: value})}
			compact
			data-flx="user.advanced-settings-tab.switch.text-selection"
		/>
	);
});

export const StayInteractiveUnfocusedControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(STAY_FULLY_INTERACTIVE_WHEN_UNFOCUSED_DESCRIPTOR)}
			value={Accessibility.stayInteractiveWhenUnfocused}
			onChange={(value) => AccessibilityCommands.update({stayInteractiveWhenUnfocused: value})}
			compact
			data-flx="user.advanced-settings-tab.switch.stay-interactive-unfocused"
		/>
	);
});

export const VideoSeekThumbnailsControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(SHOW_VIDEO_SEEK_PREVIEW_THUMBNAILS_DESCRIPTOR)}
			value={Accessibility.showVideoSeekPreviewThumbnails}
			onChange={(value) => AccessibilityCommands.update({showVideoSeekPreviewThumbnails: value})}
			compact
			data-flx="user.advanced-settings-tab.switch.video-seek-thumbnails"
		/>
	);
});

export const SmoothScrollingControl = observer(() => {
	const {i18n} = useLingui();
	const {desktopWindowBehavior, desktopWindowBehaviorBusy, updateDesktopWindowBehavior} =
		useDesktopWindowBehaviorSettings();
	const showRestartModal = useChromiumScrollingRestartModal();
	const handleChange = useCallback(
		(value: boolean) => {
			void updateDesktopWindowBehavior({smoothScrolling: value}).then((settings) => {
				if (hasDesktopSmoothScrollingPendingRestart(settings)) {
					showRestartModal();
				}
			});
		},
		[showRestartModal, updateDesktopWindowBehavior],
	);
	return (
		<Switch
			ariaLabel={i18n._(SMOOTH_SCROLLING_DESCRIPTOR)}
			value={desktopWindowBehavior?.smoothScrolling ?? true}
			disabled={desktopWindowBehaviorBusy || desktopWindowBehavior === null}
			onChange={handleChange}
			compact
			data-flx="user.advanced-settings-tab.switch.smooth-scrolling"
		/>
	);
});

export const MiddleClickAutoscrollControl = observer(() => {
	const {i18n} = useLingui();
	const {desktopWindowBehavior, desktopWindowBehaviorBusy, updateDesktopWindowBehavior} =
		useDesktopWindowBehaviorSettings();
	const showRestartModal = useChromiumScrollingRestartModal();
	const handleChange = useCallback(
		(value: boolean) => {
			void updateDesktopWindowBehavior({middleClickAutoscroll: value}).then((settings) => {
				if (hasDesktopMiddleClickAutoscrollPendingRestart(settings)) {
					showRestartModal();
				}
			});
		},
		[showRestartModal, updateDesktopWindowBehavior],
	);
	return (
		<Switch
			ariaLabel={i18n._(USE_AUTOSCROLLING_DESCRIPTOR)}
			value={desktopWindowBehavior?.middleClickAutoscroll ?? false}
			disabled={desktopWindowBehaviorBusy || desktopWindowBehavior === null}
			onChange={handleChange}
			compact
			data-flx="user.advanced-settings-tab.switch.middle-click-autoscroll"
		/>
	);
});
