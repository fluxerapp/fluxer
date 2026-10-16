// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {relaunchDesktopApp} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import {useDesktopWindowBehaviorSettings} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedDesktopControls';
import {
	ENABLE_TEXT_SELECTION_DESCRIPTOR,
	SMOOTH_SCROLLING_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {
	LATER_DESCRIPTOR,
	RESTART_NOW_DESCRIPTOR,
	RESTART_DESCRIPTOR as RESTART_PRODUCT_DESCRIPTOR,
	VIDEO_SEEK_THUMBNAILS_DESCRIPTOR as SHOW_VIDEO_SEEK_PREVIEW_THUMBNAILS_DESCRIPTOR,
	STAY_FULLY_INTERACTIVE_WHEN_UNFOCUSED_DESCRIPTOR,
	USE_AUTOSCROLLING_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import type {DesktopWindowBehaviorSettings} from '@app/types/electron.d';
import {ph} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

function hasDesktopSmoothScrollingPendingRestart(settings: DesktopWindowBehaviorSettings | null): boolean {
	return settings !== null && settings.smoothScrolling !== settings.activeSmoothScrolling;
}

function hasDesktopMiddleClickAutoscrollPendingRestart(settings: DesktopWindowBehaviorSettings | null): boolean {
	return settings !== null && settings.middleClickAutoscroll !== settings.activeMiddleClickAutoscroll;
}

function useChromiumScrollingRestartModal() {
	const {i18n} = useLingui();
	return useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(RESTART_PRODUCT_DESCRIPTOR, {productName: RuntimeConfig.productName})}
					description={
						<Trans>
							{ph({PRODUCT_NAME: RuntimeConfig.productName})} needs to restart before Chromium's native scrolling
							settings change how scrolling works.
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
