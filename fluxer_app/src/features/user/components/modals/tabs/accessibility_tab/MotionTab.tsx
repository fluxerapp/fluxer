// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const SYNC_REDUCED_MOTION_SETTING_WITH_SYSTEM_DESCRIPTOR = msg({
	message: 'Sync reduced motion setting with system',
	comment: 'Label in the motion tab.',
});
const AUTOMATICALLY_USE_YOUR_SYSTEM_S_REDUCED_MOTION_PREFERENCE_DESCRIPTOR = msg({
	message: "Use this device's system reduced motion preference, or customize it below.",
	comment: 'Description text in the motion tab.',
});
const REDUCE_MOTION_DESCRIPTOR = msg({
	message: 'Reduce motion',
	comment: 'Short label in the motion tab. Keep it concise.',
});
const DISABLE_ANIMATIONS_AND_TRANSITIONS_CURRENTLY_CONTROLLED_BY_YOUR_DESCRIPTOR = msg({
	message: 'Disable animations and transitions. Currently controlled by your system setting.',
	comment: 'Button or menu action label in the motion tab. Keep it concise.',
});
const DISABLE_ANIMATIONS_AND_TRANSITIONS_THROUGHOUT_THE_APP_DESCRIPTOR = msg({
	message: 'Disable animations and transitions throughout the app.',
	comment: 'Button or menu action label in the motion tab. Keep it concise.',
});
const CONTENT_ANIMATIONS_STAY_IN_ANIMATION_TAB_DESCRIPTOR = msg({
	message: 'Animated emojis, GIFs and stickers stay under your control in the Animation tab.',
	comment: 'Extra hint shown beneath the reduce motion toggle while reduced motion is active.',
});
const STAY_FULLY_INTERACTIVE_WHEN_UNFOCUSED_DESCRIPTOR = msg({
	message: 'Stay fully interactive when unfocused',
	comment: 'Label in the motion tab.',
});
const KEEP_ANIMATIONS_GIF_PLAYBACK_HOVER_EFFECTS_AND_TOOLTIPS_RUNNING_DESCRIPTOR = msg({
	message: 'Keep animations, GIF playback, hover effects, and tooltips running while the window is unfocused.',
	comment: 'Description text in the motion tab.',
});
export const MotionTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const syncReducedMotionWithSystem = Accessibility.syncReducedMotionWithSystem;
	const reducedMotionOverride = Accessibility.reducedMotionOverride;
	const useReducedMotion = Accessibility.useReducedMotion;
	const reduceMotionBaseDescription = syncReducedMotionWithSystem
		? i18n._(DISABLE_ANIMATIONS_AND_TRANSITIONS_CURRENTLY_CONTROLLED_BY_YOUR_DESCRIPTOR)
		: i18n._(DISABLE_ANIMATIONS_AND_TRANSITIONS_THROUGHOUT_THE_APP_DESCRIPTOR);
	const reduceMotionDescription = useReducedMotion
		? `${reduceMotionBaseDescription} ${i18n._(CONTENT_ANIMATIONS_STAY_IN_ANIMATION_TAB_DESCRIPTOR)}`
		: reduceMotionBaseDescription;
	return (
		<>
			<Switch
				label={i18n._(SYNC_REDUCED_MOTION_SETTING_WITH_SYSTEM_DESCRIPTOR)}
				description={i18n._(AUTOMATICALLY_USE_YOUR_SYSTEM_S_REDUCED_MOTION_PREFERENCE_DESCRIPTOR)}
				value={syncReducedMotionWithSystem}
				onChange={(value) => AccessibilityCommands.update({syncReducedMotionWithSystem: value})}
				data-flx="user.accessibility-tab.motion-tab.motion-tab-content.switch.update"
			/>
			<Switch
				label={i18n._(REDUCE_MOTION_DESCRIPTOR)}
				description={reduceMotionDescription}
				value={syncReducedMotionWithSystem ? useReducedMotion : (reducedMotionOverride ?? false)}
				disabled={syncReducedMotionWithSystem}
				onChange={(value) => AccessibilityCommands.update({reducedMotionOverride: value})}
				data-flx="user.accessibility-tab.motion-tab.motion-tab-content.switch.update--2"
			/>
			<Switch
				label={i18n._(STAY_FULLY_INTERACTIVE_WHEN_UNFOCUSED_DESCRIPTOR)}
				description={i18n._(KEEP_ANIMATIONS_GIF_PLAYBACK_HOVER_EFFECTS_AND_TOOLTIPS_RUNNING_DESCRIPTOR)}
				value={Accessibility.stayInteractiveWhenUnfocused}
				onChange={(value) => AccessibilityCommands.update({stayInteractiveWhenUnfocused: value})}
				data-flx="user.accessibility-tab.motion-tab.motion-tab-content.switch.update--3"
			/>
		</>
	);
});
