// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {CUSTOM_SOUND_MAX_SIZE_LABEL} from '@app/features/app/config/I18nDisplayConstants';
import type * as CustomSoundDB from '@app/features/notification/utils/CustomSoundDB';
import {type SoundType, SoundType as SoundTypes} from '@app/features/notification/utils/SoundUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Accordion} from '@app/features/ui/accordion/Accordion';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {Slider} from '@app/features/ui/components/Slider';
import {SwitchGroup, SwitchGroupCustomItem} from '@app/features/ui/components/SwitchGroup';
import {SliderResetIconButton} from '@app/features/ui/components/slider/SliderResetIconButton';
import type {SoundSettings} from '@app/features/ui/state/Sound';
import {formatRoundedPercentage, roundPercentage} from '@app/features/ui/utils/PercentageFormatting';
import styles from '@app/features/user/components/modals/tabs/notifications_tab/NotificationsTabSounds.module.css';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CaretDownIcon, SpeakerHighIcon, SpeakerXIcon, TrashIcon, UploadIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useId, useRef, useState} from 'react';

const MASTER_VOLUME_DESCRIPTOR = msg({
	message: 'Master volume',
	comment: 'Short label in the sounds. Keep it concise.',
});
const SETS_THE_LEVEL_FOR_EVERY_SOUND_EFFECT_PER_DESCRIPTOR = msg({
	message: 'Sets the level for every sound effect. Per-sound overrides ignore this.',
	comment: 'Description text in the sounds.',
});
const RESET_TO_DEFAULT_VOLUME_DESCRIPTOR = msg({
	message: 'Reset to default volume',
	comment: 'Button or menu action label in the sounds. Keep it concise.',
});
const DISABLE_ALL_NOTIFICATION_SOUNDS_DESCRIPTOR = msg({
	message: 'Disable all notification sounds',
	comment: 'Button or menu action label in the sounds. Keep it concise.',
});
const YOUR_EXISTING_NOTIFICATION_SOUND_SETTINGS_WILL_BE_PRESERVED_DESCRIPTOR = msg({
	message: 'Your existing notification sound settings will be preserved.',
	comment: 'Description text in the sounds.',
});
const CLICK_THE_UPLOAD_ICON_NEXT_TO_ANY_SOUND_DESCRIPTOR = msg({
	message:
		'Click the upload icon next to any sound to customize it with your own audio file. Max {customSoundMaxSizeLabel}.',
	comment: 'Label in the sounds.',
});
const SHOW_MORE_SOUND_EFFECTS_DESCRIPTOR = msg({
	message: 'Show more sound effects',
	comment: 'Button label in the sounds section for revealing less common sound effects.',
});
const SHOW_FEWER_SOUND_EFFECTS_DESCRIPTOR = msg({
	message: 'Show fewer sound effects',
	comment: 'Button label in the sounds section for hiding less common sound effects.',
});
const PREVIEW_SOUND_DESCRIPTOR = msg({
	message: 'Preview sound',
	comment: 'Short label in the sounds. Keep it concise.',
});
const UPLOAD_CUSTOM_SOUND_FOR_DESCRIPTOR = msg({
	message: 'Upload custom sound for {label}',
	comment: 'Button or menu action label in the sounds. Keep it concise. Preserve {label}; it is inserted by code.',
});
const REMOVE_CUSTOM_SOUND_FOR_DESCRIPTOR = msg({
	message: 'Remove custom sound for {label}',
	comment:
		'Button or menu action label in the sounds. Keep it concise. Preserve {label}; it is inserted by code. Keep the tone plain and specific.',
});
const PER_SOUND_VOLUME_DESCRIPTOR = msg({
	message: 'Per-sound volume',
	comment: 'Short label in the sounds. Keep it concise.',
});
const SOUND_S_OVERRIDE_THE_MASTER_VOLUME_DESCRIPTOR = msg({
	message: 'Active custom sound volume overrides: {overrideCount}.',
	comment: 'Description text in the sounds. Preserve {overrideCount}; it is inserted by code.',
});
const SET_CUSTOM_VOLUMES_FOR_INDIVIDUAL_SOUNDS_SOUNDS_WITHOUT_DESCRIPTOR = msg({
	message: 'Set custom volumes for individual sounds. Sounds without an override follow the master volume.',
	comment: 'Description text in the sounds.',
});
const FOLLOWING_MASTER_DESCRIPTOR = msg({
	message: 'Following master • {effectiveValue}%',
	comment: 'Label in the sounds. Preserve {effectiveValue}; it is inserted by code.',
});
const RESET_TO_MASTER_VOLUME_DESCRIPTOR = msg({
	message: 'Reset {label} to master volume',
	comment: 'Button or menu action label in the sounds. Keep it concise. Preserve {label}; it is inserted by code.',
});
const RESET_ALL_OVERRIDES_DESCRIPTOR = msg({
	message: 'Reset all overrides',
	comment: 'Button or menu action label in the sounds. Keep it concise.',
});
const MUTE_SOUND_DESCRIPTOR = msg({
	message: 'Mute {label}',
	comment: 'Button label for disabling one notification sound. Preserve {label}; it is inserted by code.',
});
const UNMUTE_SOUND_DESCRIPTOR = msg({
	message: 'Unmute {label}',
	comment: 'Button label for enabling one notification sound. Preserve {label}; it is inserted by code.',
});

interface SoundsProps {
	soundSettings: SoundSettings;
	soundTypeLabels: Record<SoundType, string>;
	customSounds: Record<SoundType, CustomSoundDB.CustomSound | null>;
	isSoundEnabled: (soundType: SoundType) => boolean;
	onToggleAllSounds: (value: boolean) => void;
	onToggleSound: (soundType: SoundType, enabled: boolean) => void;
	onPreviewSound: (soundType: SoundType) => void;
	onUploadClick: (soundType: SoundType) => void;
	onCustomSoundDelete: (soundType: SoundType) => void;
	onMasterVolumeChange: (value: number) => void;
	onSoundOverrideChange: (soundType: SoundType, value: number) => void;
	onSoundOverrideReset: (soundType: SoundType) => void;
	onAllOverridesReset: () => void;
}

const DEFAULT_MASTER = 100;
const COMMON_SOUND_TYPES = new Set<SoundType>([
	SoundTypes.Message,
	SoundTypes.DirectMessage,
	SoundTypes.SameChannelMessage,
	SoundTypes.IncomingRing,
	SoundTypes.Mute,
	SoundTypes.Unmute,
	SoundTypes.Deaf,
	SoundTypes.Undeaf,
]);
const SOUND_LIST_EXPAND_TRANSITION = {
	duration: 0.08,
	ease: 'easeOut' as const,
};
const SOUND_LIST_INSTANT_TRANSITION = {duration: 0};
export const Sounds: React.FC<SoundsProps> = observer(
	({
		soundSettings,
		soundTypeLabels,
		customSounds,
		isSoundEnabled,
		onToggleAllSounds,
		onToggleSound,
		onPreviewSound,
		onUploadClick,
		onCustomSoundDelete,
		onMasterVolumeChange,
		onSoundOverrideChange,
		onSoundOverrideReset,
		onAllOverridesReset,
	}) => {
		const {i18n} = useLingui();
		const masterVolume = soundSettings.masterVolume ?? DEFAULT_MASTER;
		const soundOverrides = soundSettings.soundOverrides ?? {};
		const overrideCount = Object.keys(soundOverrides).length;
		const hasAnyOverride = overrideCount > 0;
		const allDisabled = soundSettings.allSoundsDisabled;
		const reducedMotion = Accessibility.useReducedMotion;
		const [accordionExpanded, setAccordionExpanded] = useState(hasAnyOverride);
		const [allSoundsExpanded, setAllSoundsExpanded] = useState(false);
		const previousHadOverridesRef = useRef(hasAnyOverride);
		const additionalSoundsId = useId();
		const soundEntries = Object.entries(soundTypeLabels).map(
			([soundType, label]) => [soundType as SoundType, label] as const,
		);
		const commonSoundEntries = soundEntries.filter(([soundType]) => COMMON_SOUND_TYPES.has(soundType));
		const additionalSoundEntries = soundEntries.filter(([soundType]) => !COMMON_SOUND_TYPES.has(soundType));
		const hasAdditionalSounds = additionalSoundEntries.length > 0;
		const soundListTransition = reducedMotion ? SOUND_LIST_INSTANT_TRANSITION : SOUND_LIST_EXPAND_TRANSITION;
		const masterVolumeAtDefault = masterVolume === DEFAULT_MASTER;
		const renderSoundToggle = ([type, label]: readonly [SoundType, string]) => (
			<SwitchGroupCustomItem
				key={type}
				label={
					<div className={styles.soundLabel} data-flx="user.notifications-tab.sounds.sound-label">
						<div className={styles.soundLabelTitle} data-flx="user.notifications-tab.sounds.sound-label-title">
							<span data-flx="user.notifications-tab.sounds.span">{label}</span>
							{customSounds[type] && (
								<span className={styles.customBadge} data-flx="user.notifications-tab.sounds.custom-badge">
									<Trans>Custom</Trans>
								</span>
							)}
						</div>
						<button
							type="button"
							className={styles.previewLink}
							onClick={() => onPreviewSound(type)}
							disabled={allDisabled}
							data-flx="user.notifications-tab.sounds.preview-link.preview-sound.button"
						>
							{i18n._(PREVIEW_SOUND_DESCRIPTOR)}
						</button>
					</div>
				}
				value={allDisabled ? false : isSoundEnabled(type)}
				disabled={allDisabled}
				onChange={(enabled) => onToggleSound(type, enabled)}
				extraContent={
					<>
						<button
							type="button"
							onClick={() => onUploadClick(type)}
							disabled={allDisabled}
							className={styles.iconButton}
							aria-label={i18n._(UPLOAD_CUSTOM_SOUND_FOR_DESCRIPTOR, {label})}
							data-flx="user.notifications-tab.sounds.icon-button.upload-click"
						>
							<UploadIcon
								size={remFromPx(16)}
								className={styles.uploadIcon}
								data-flx="user.notifications-tab.sounds.upload-icon"
							/>
						</button>
						{customSounds[type] && (
							<button
								type="button"
								onClick={() => onCustomSoundDelete(type)}
								disabled={allDisabled}
								className={styles.iconButton}
								aria-label={i18n._(REMOVE_CUSTOM_SOUND_FOR_DESCRIPTOR, {label})}
								data-flx="user.notifications-tab.sounds.icon-button.custom-sound-delete"
							>
								<TrashIcon
									size={remFromPx(16)}
									className={styles.deleteIcon}
									data-flx="user.notifications-tab.sounds.delete-icon"
								/>
							</button>
						)}
					</>
				}
				data-flx="user.notifications-tab.sounds.switch-group-custom-item.toggle-sound"
			/>
		);
		useEffect(() => {
			if (hasAnyOverride && !previousHadOverridesRef.current) {
				setAccordionExpanded(true);
			}
			previousHadOverridesRef.current = hasAnyOverride;
		}, [hasAnyOverride]);
		return (
			<div className={styles.container} data-flx="user.notifications-tab.sounds.container">
				<div className={styles.content} data-flx="user.notifications-tab.sounds.content">
					<div className={styles.masterRow} data-flx="user.notifications-tab.sounds.master-row">
						<div className={styles.masterHeader} data-flx="user.notifications-tab.sounds.master-header">
							<span className={styles.masterLabel} data-flx="user.notifications-tab.sounds.master-label">
								{i18n._(MASTER_VOLUME_DESCRIPTOR)}
							</span>
							<SliderResetIconButton
								canReset={!allDisabled && !masterVolumeAtDefault}
								onReset={() => onMasterVolumeChange(DEFAULT_MASTER)}
								ariaLabel={i18n._(RESET_TO_DEFAULT_VOLUME_DESCRIPTOR)}
								className={styles.masterResetButton}
								iconSize={16}
								dataFlx="user.notifications-tab.sounds.reset-button.master-volume-reset"
								data-flx="user.notifications-tab.notifications-tab-sounds.sounds.master-reset-button"
							/>
						</div>
						<p className={styles.masterDescription} data-flx="user.notifications-tab.sounds.master-description">
							{i18n._(SETS_THE_LEVEL_FOR_EVERY_SOUND_EFFECT_PER_DESCRIPTOR)}
						</p>
						<Slider
							value={masterVolume}
							defaultValue={masterVolume}
							factoryDefaultValue={DEFAULT_MASTER}
							minValue={0}
							maxValue={200}
							step={1}
							markers={[0, 50, 100, 150, 200]}
							disabled={allDisabled}
							onMarkerRender={formatRoundedPercentage}
							onValueRender={formatRoundedPercentage}
							onValueChange={onMasterVolumeChange}
							data-flx="user.notifications-tab.sounds.slider"
						/>
					</div>
					<Switch
						label={i18n._(DISABLE_ALL_NOTIFICATION_SOUNDS_DESCRIPTOR)}
						description={i18n._(YOUR_EXISTING_NOTIFICATION_SOUND_SETTINGS_WILL_BE_PRESERVED_DESCRIPTOR)}
						value={allDisabled}
						onChange={onToggleAllSounds}
						data-flx="user.notifications-tab.sounds.switch.toggle-all-sounds"
					/>
					<div className={styles.hint} data-flx="user.notifications-tab.sounds.hint">
						{i18n._(CLICK_THE_UPLOAD_ICON_NEXT_TO_ANY_SOUND_DESCRIPTOR, {
							customSoundMaxSizeLabel: CUSTOM_SOUND_MAX_SIZE_LABEL,
						})}
					</div>
					<div
						className={clsx(
							styles.soundListFrame,
							hasAdditionalSounds && !allSoundsExpanded && styles.soundListFrameTruncated,
						)}
						data-flx="user.notifications-tab.sounds.sound-list-frame"
					>
						<SwitchGroup data-flx="user.notifications-tab.sounds.switch-group">
							{commonSoundEntries.map(renderSoundToggle)}
							<AnimatePresence initial={false} data-flx="user.notifications-tab.sounds.animate-presence">
								{allSoundsExpanded && (
									<motion.div
										key="additional-sounds"
										id={additionalSoundsId}
										className={styles.extraSounds}
										initial={{height: 0, opacity: 0}}
										animate={{height: 'auto', opacity: 1}}
										exit={{height: 0, opacity: 0}}
										transition={soundListTransition}
										data-flx="user.notifications-tab.sounds.extra-sounds"
									>
										{additionalSoundEntries.map(renderSoundToggle)}
									</motion.div>
								)}
							</AnimatePresence>
						</SwitchGroup>
						{hasAdditionalSounds && !allSoundsExpanded && (
							<div className={styles.soundListFade} aria-hidden={true} data-flx="user.notifications-tab.sounds.fade" />
						)}
					</div>
					{hasAdditionalSounds && (
						<div className={styles.revealRow} data-flx="user.notifications-tab.sounds.reveal-row">
							<button
								type="button"
								className={styles.revealButton}
								aria-expanded={allSoundsExpanded}
								aria-controls={additionalSoundsId}
								onClick={() => setAllSoundsExpanded((expanded) => !expanded)}
								data-flx="user.notifications-tab.sounds.reveal-button.toggle-all-sounds.button"
							>
								<span data-flx="user.notifications-tab.sounds.reveal-label">
									{i18n._(allSoundsExpanded ? SHOW_FEWER_SOUND_EFFECTS_DESCRIPTOR : SHOW_MORE_SOUND_EFFECTS_DESCRIPTOR)}
								</span>
								<motion.span
									className={styles.revealChevron}
									aria-hidden={true}
									animate={{rotate: allSoundsExpanded ? 180 : 0, y: allSoundsExpanded ? -1 : 0}}
									transition={soundListTransition}
									data-flx="user.notifications-tab.sounds.reveal-chevron"
								>
									<CaretDownIcon
										size={remFromPx(16)}
										weight="bold"
										data-flx="user.notifications-tab.sounds.caret-down-icon"
									/>
								</motion.span>
							</button>
						</div>
					)}
					<Accordion
						id="sound-overrides"
						title={i18n._(PER_SOUND_VOLUME_DESCRIPTOR)}
						description={
							hasAnyOverride
								? i18n._(SOUND_S_OVERRIDE_THE_MASTER_VOLUME_DESCRIPTOR, {overrideCount})
								: i18n._(SET_CUSTOM_VOLUMES_FOR_INDIVIDUAL_SOUNDS_SOUNDS_WITHOUT_DESCRIPTOR)
						}
						expanded={accordionExpanded}
						onExpandedChange={setAccordionExpanded}
						data-flx="user.notifications-tab.sounds.sound-overrides"
					>
						<div className={styles.overridesList} data-flx="user.notifications-tab.sounds.overrides-list">
							{Object.entries(soundTypeLabels).map(([soundType, label]) => {
								const type = soundType as SoundType;
								const override = soundOverrides[type];
								const hasOverride = override !== undefined;
								const effectiveValue = hasOverride ? (override as number) : masterVolume;
								const effectiveValueLabel = formatRoundedPercentage(effectiveValue);
								const soundEnabled = isSoundEnabled(type);
								const rowDisabled = allDisabled || !soundEnabled;
								const SoundToggleIcon = soundEnabled ? SpeakerHighIcon : SpeakerXIcon;
								return (
									<div
										key={soundType}
										className={clsx(styles.overrideRow, !hasOverride && styles.overrideRowLinked)}
										data-flx="user.notifications-tab.sounds.override-row"
									>
										<div className={styles.overrideHeader} data-flx="user.notifications-tab.sounds.override-header">
											<span className={styles.overrideLabel} data-flx="user.notifications-tab.sounds.override-label">
												{label}
											</span>
											<button
												type="button"
												className={clsx(styles.soundToggleButton, !soundEnabled && styles.soundToggleButtonMuted)}
												onClick={() => onToggleSound(type, !soundEnabled)}
												disabled={allDisabled}
												aria-pressed={!soundEnabled}
												aria-label={i18n._(soundEnabled ? MUTE_SOUND_DESCRIPTOR : UNMUTE_SOUND_DESCRIPTOR, {label})}
												data-flx="user.notifications-tab.sounds.sound-toggle-button.toggle-sound"
											>
												<SoundToggleIcon
													size={remFromPx(14)}
													weight="fill"
													data-flx="user.notifications-tab.sounds.sound-toggle-icon"
												/>
											</button>
											<span className={styles.overrideStatus} data-flx="user.notifications-tab.sounds.override-status">
												{hasOverride
													? effectiveValueLabel
													: i18n._(FOLLOWING_MASTER_DESCRIPTOR, {effectiveValue: roundPercentage(effectiveValue)})}
											</span>
											<SliderResetIconButton
												canReset={!rowDisabled && hasOverride}
												onReset={() => onSoundOverrideReset(type)}
												ariaLabel={i18n._(RESET_TO_MASTER_VOLUME_DESCRIPTOR, {label})}
												dataFlx="user.notifications-tab.sounds.reset-button.sound-override-reset"
												data-flx="user.notifications-tab.notifications-tab-sounds.sounds.slider-reset-icon-button"
											/>
										</div>
										<Slider
											value={effectiveValue}
											defaultValue={effectiveValue}
											factoryDefaultValue={masterVolume}
											minValue={0}
											maxValue={200}
											step={1}
											disabled={rowDisabled}
											className={styles.overrideSlider}
											onValueRender={formatRoundedPercentage}
											onValueChange={(value) => onSoundOverrideChange(type, value)}
											data-flx="user.notifications-tab.sounds.slider--2"
										/>
									</div>
								);
							})}
						</div>
						{hasAnyOverride && !allDisabled && (
							<div className={styles.actionsContainer} data-flx="user.notifications-tab.sounds.actions-container--2">
								<button
									type="button"
									className={styles.actionButton}
									onClick={onAllOverridesReset}
									data-flx="user.notifications-tab.sounds.action-button.all-overrides-reset"
								>
									{i18n._(RESET_ALL_OVERRIDES_DESCRIPTOR)}
								</button>
							</div>
						)}
					</Accordion>
				</div>
			</div>
		);
	},
);
