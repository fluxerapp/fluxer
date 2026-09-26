// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {resolveMotionPreferencesModel} from '@app/features/accessibility/state/MotionPreferencesMachine';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import styles from '@app/features/user/components/modals/tabs/accessibility_tab/AnimationTab.module.css';
import UserSettings from '@app/features/user/state/UserSettings';
import {StickerAnimationOptions} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const ALWAYS_ANIMATE_DESCRIPTOR = msg({
	message: 'Always animate',
	comment: 'Short label in the animation tab. Keep it concise.',
});
const STICKERS_WILL_ALWAYS_ANIMATE_DESCRIPTOR = msg({
	message: 'Stickers will always animate',
	comment: 'Label in the animation tab.',
});
const ANIMATE_ON_INTERACTION_DESCRIPTOR = msg({
	message: 'Animate on interaction',
	comment: 'Short label in the animation tab. Keep it concise.',
});
const STICKERS_WILL_ANIMATE_WHEN_YOU_PRESS_THEM_DESCRIPTOR = msg({
	message: 'Stickers will animate when you press them',
	comment: 'Label in the animation tab.',
});
const STICKERS_WILL_ANIMATE_WHEN_YOU_HOVER_OR_INTERACT_DESCRIPTOR = msg({
	message: 'Stickers will animate when you hover or interact with them',
	comment: 'Label in the animation tab.',
});
const NEVER_ANIMATE_DESCRIPTOR = msg({
	message: 'Never animate',
	comment: 'Short label in the animation tab. Keep it concise.',
});
const STICKERS_WILL_NEVER_ANIMATE_DESCRIPTOR = msg({
	message: 'Stickers will never animate',
	comment: 'Label in the animation tab.',
});
const PLAY_ANIMATED_EMOJIS_DESCRIPTOR = msg({
	message: 'Play animated emojis',
	comment: 'Short label in the animation tab. Keep it concise.',
});
const AUTOMATICALLY_PLAY_GIFS_DESCRIPTOR = msg({
	message: 'Automatically play GIFs',
	comment: 'Short label in the animation tab. Keep it concise.',
});
const AUTOMATICALLY_PLAY_GIFS_WHEN_IS_FOCUSED_DESCRIPTOR = msg({
	message: 'Automatically play GIFs when {productName} is focused',
	comment: 'Label in the animation tab. Preserve {productName}; it is inserted by code.',
});
const DEFAULTS_TO_OFF_ON_MOBILE_TO_PRESERVE_BATTERY_DESCRIPTOR = msg({
	message: 'Defaults to off on mobile to preserve battery life and reduce data usage.',
	comment: 'Description text in the animation tab.',
});
const DEFAULTS_TO_ANIMATE_ON_INTERACTION_ON_MOBILE_TO_DESCRIPTOR = msg({
	message: 'Defaults to animating on interaction on mobile to preserve battery life.',
	comment: 'Description text in the animation tab.',
});
const STICKER_ANIMATION_PREFERENCE_DESCRIPTOR = msg({
	message: 'Sticker animation preference',
	comment: 'Short label in the animation tab. Keep it concise.',
});
const REDUCED_MOTION_ACTIVE_NOTE_DESCRIPTOR = msg({
	message:
		'Reduced motion is on, so content animations are paused by default. You can still turn any of these back on to keep them playing.',
	comment: 'Explanatory note shown above the animation controls when reduced motion is active.',
});
const PLAYING_DESPITE_REDUCED_MOTION_DESCRIPTOR = msg({
	message: 'Playing despite reduced motion.',
	comment: 'Description shown when an animation setting overrides reduced motion.',
});
const PAUSED_BY_REDUCED_MOTION_EMOJI_DESCRIPTOR = msg({
	message: 'Paused by reduced motion. Turn on to keep animated emojis playing.',
	comment: 'Description shown for the emoji toggle while reduced motion is active.',
});
const PAUSED_BY_REDUCED_MOTION_GIF_DESCRIPTOR = msg({
	message: 'Paused by reduced motion. Turn on to keep GIFs playing.',
	comment: 'Description shown for the GIF toggle while reduced motion is active.',
});
const STICKERS_ALWAYS_ANIMATE_DESPITE_REDUCED_MOTION_DESCRIPTOR = msg({
	message: 'Always animating despite reduced motion.',
	comment: 'Description shown for stickers when always-animate overrides reduced motion.',
});
const STICKERS_REDUCED_MOTION_HINT_DESCRIPTOR = msg({
	message: 'With reduced motion on, stickers only animate on interaction. Choose "Always animate" to override.',
	comment: 'Description shown for the sticker control while reduced motion is active.',
});

export const AnimationTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const mobileLayout = MobileLayout;
	const mobileStickerAnimationOverridden = Accessibility.mobileStickerAnimationOverridden;
	const mobileGifAutoPlayOverridden = Accessibility.mobileGifAutoPlayOverridden;
	const motion = resolveMotionPreferencesModel(UserSettings.getMotionPreferencesInput());
	const stickerAnimationOptions = useMemo(
		() =>
			[
				{
					value: StickerAnimationOptions.ALWAYS_ANIMATE,
					name: i18n._(ALWAYS_ANIMATE_DESCRIPTOR),
					desc: i18n._(STICKERS_WILL_ALWAYS_ANIMATE_DESCRIPTOR),
				},
				{
					value: StickerAnimationOptions.ANIMATE_ON_INTERACTION,
					name: i18n._(ANIMATE_ON_INTERACTION_DESCRIPTOR),
					desc: mobileLayout.enabled
						? i18n._(STICKERS_WILL_ANIMATE_WHEN_YOU_PRESS_THEM_DESCRIPTOR)
						: i18n._(STICKERS_WILL_ANIMATE_WHEN_YOU_HOVER_OR_INTERACT_DESCRIPTOR),
				},
				{
					value: StickerAnimationOptions.NEVER_ANIMATE,
					name: i18n._(NEVER_ANIMATE_DESCRIPTOR),
					desc: i18n._(STICKERS_WILL_NEVER_ANIMATE_DESCRIPTOR),
				},
			] as ReadonlyArray<RadioOption<number>>,
		[mobileLayout.enabled, i18n.locale],
	);
	const emojiDescription = (): string | undefined => {
		if (motion.emojiOverridesReducedMotion) return i18n._(PLAYING_DESPITE_REDUCED_MOTION_DESCRIPTOR);
		if (motion.reducedMotion) return i18n._(PAUSED_BY_REDUCED_MOTION_EMOJI_DESCRIPTOR);
		return;
	};
	const gifDescription = (): string | undefined => {
		if (motion.gifOverridesReducedMotion) return i18n._(PLAYING_DESPITE_REDUCED_MOTION_DESCRIPTOR);
		if (motion.reducedMotion) return i18n._(PAUSED_BY_REDUCED_MOTION_GIF_DESCRIPTOR);
		if (mobileLayout.enabled && !mobileGifAutoPlayOverridden)
			return i18n._(DEFAULTS_TO_OFF_ON_MOBILE_TO_PRESERVE_BATTERY_DESCRIPTOR);
		return;
	};
	const stickerDescription = (): string | undefined => {
		if (motion.stickerOverridesReducedMotion) return i18n._(STICKERS_ALWAYS_ANIMATE_DESPITE_REDUCED_MOTION_DESCRIPTOR);
		if (motion.reducedMotion) return i18n._(STICKERS_REDUCED_MOTION_HINT_DESCRIPTOR);
		if (mobileLayout.enabled && !mobileStickerAnimationOverridden)
			return i18n._(DEFAULTS_TO_ANIMATE_ON_INTERACTION_ON_MOBILE_TO_DESCRIPTOR);
		return;
	};
	const stickerDescriptionText = stickerDescription();
	return (
		<>
			{motion.reducedMotion && (
				<p
					className={styles.reducedMotionNote}
					data-flx="user.accessibility-tab.animation-tab.animation-tab-content.reduced-motion-note"
				>
					{i18n._(REDUCED_MOTION_ACTIVE_NOTE_DESCRIPTOR)}
				</p>
			)}
			<Switch
				label={i18n._(PLAY_ANIMATED_EMOJIS_DESCRIPTOR)}
				description={emojiDescription()}
				value={motion.effectiveAnimateEmoji}
				onChange={(value) => AccessibilityCommands.requestAnimateEmoji(value)}
				data-flx="user.accessibility-tab.animation-tab.animation-tab-content.switch"
			/>
			<Switch
				label={
					mobileLayout.enabled
						? i18n._(AUTOMATICALLY_PLAY_GIFS_DESCRIPTOR)
						: i18n._(AUTOMATICALLY_PLAY_GIFS_WHEN_IS_FOCUSED_DESCRIPTOR, {productName: PRODUCT_NAME})
				}
				description={gifDescription()}
				value={motion.effectiveGifAutoPlay}
				onChange={(value) => AccessibilityCommands.requestGifAutoPlay(value)}
				data-flx="user.accessibility-tab.animation-tab.animation-tab-content.switch--2"
			/>
			<div
				className={styles.radioSection}
				data-flx="user.accessibility-tab.animation-tab.animation-tab-content.radio-section"
			>
				<div
					className={styles.radioHeader}
					data-flx="user.accessibility-tab.animation-tab.animation-tab-content.radio-header"
				>
					<div
						className={styles.radioLabel}
						data-flx="user.accessibility-tab.animation-tab.animation-tab-content.radio-label"
					>
						<Trans>Sticker animations</Trans>
					</div>
					{stickerDescriptionText && (
						<p
							className={styles.radioDescription}
							data-flx="user.accessibility-tab.animation-tab.animation-tab-content.radio-description"
						>
							{stickerDescriptionText}
						</p>
					)}
				</div>
				<RadioGroup
					aria-label={i18n._(STICKER_ANIMATION_PREFERENCE_DESCRIPTOR)}
					options={stickerAnimationOptions}
					value={motion.effectiveAnimateStickers}
					onChange={(value) => AccessibilityCommands.requestAnimateStickers(value)}
					data-flx="user.accessibility-tab.animation-tab.animation-tab-content.radio-group"
				/>
			</div>
		</>
	);
});
