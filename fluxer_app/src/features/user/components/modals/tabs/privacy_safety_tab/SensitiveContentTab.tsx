// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {getNextTabIndex, getTabNavigationDirection} from '@app/features/ui/tabs/TabKeyboardNavigation';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import styles from '@app/features/user/components/modals/tabs/privacy_safety_tab/SensitiveContentTab.module.css';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import {SensitiveMediaFilterLevel} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useId, useMemo, useRef, useState} from 'react';

const SHOW_DESCRIPTOR = msg({
	message: 'Show',
	context: 'sensitive-media-filter-level',
	comment:
		'Sensitive media filter option meaning the media is shown as-is. One of three mutually exclusive values (Show, Blur, Block); keep all three in the same grammatical form.',
});
const BLUR_DESCRIPTOR = msg({
	message: 'Blur',
	context: 'sensitive-media-filter-level',
	comment:
		'Sensitive media filter option meaning the media is blurred until the viewer reveals it. One of three mutually exclusive values (Show, Blur, Block); keep all three in the same grammatical form.',
});
const BLOCK_DESCRIPTOR = msg({
	message: 'Block',
	context: 'sensitive-media-filter-level',
	comment:
		'Sensitive media filter option meaning the media is hidden entirely. One of three mutually exclusive values (Show, Blur, Block); keep all three in the same grammatical form.',
});
const DIRECT_MESSAGES_FROM_FRIENDS_DESCRIPTOR = msg({
	message: 'Direct messages from friends',
	comment: 'Label in the sensitive content tab.',
});
const DIRECT_MESSAGES_FROM_OTHERS_DESCRIPTOR = msg({
	message: 'Direct messages from others',
	comment: 'Label in the sensitive content tab.',
});
const MESSAGES_IN_COMMUNITY_CHANNELS_DESCRIPTOR = msg({
	message: 'Messages in community channels',
	comment: 'Label in the sensitive content tab.',
});
const INTRO_DESCRIPTOR = msg({
	message:
		'Choose what happens to images and videos flagged as sensitive. Show displays them as usual, Blur hides them until you click, and Block hides them completely.',
	comment:
		'Introduction at the top of the sensitive content settings. Show, Blur and Block are the option names used below and must match their translations.',
});
const FRIENDS_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Media your friends send you in direct messages.',
	comment: 'Helper text under "Direct messages from friends" in the sensitive content settings.',
});
const OTHERS_DESCRIPTION_DESCRIPTOR = msg({
	message: "Media in group chats and in direct messages from people who aren't your friends.",
	comment: 'Helper text under "Direct messages from others" in the sensitive content settings.',
});
const COMMUNITY_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Media in channels that are not marked 18+. Channels marked 18+ always show it.',
	comment: 'Helper text under "Messages in community channels" in the sensitive content settings.',
});
const SENSITIVE_CONTENT_TAB_ID = 'privacy_safety';

interface SensitiveContentOption {
	value: number;
	label: string;
	disabled?: boolean;
}

interface SensitiveContentChoiceRowProps {
	label: string;
	description: string;
	value: number;
	options: ReadonlyArray<SensitiveContentOption>;
	onChange: (value: number) => void;
	disabled?: boolean;
	dataFlx: string;
}

const SensitiveContentChoiceRow: React.FC<SensitiveContentChoiceRowProps> = ({
	label,
	description,
	value,
	options,
	onChange,
	disabled,
	dataFlx,
}) => {
	const labelId = useId();
	const descriptionId = useId();
	const optionRefs = useRef(new Map<number, HTMLButtonElement>());
	const selectedIndex = options.findIndex((option) => option.value === value);
	const enabledOptions = options.filter((option) => !option.disabled);
	const focusedValue = enabledOptions.some((option) => option.value === value) ? value : enabledOptions[0]?.value;
	const handleKeyDown = (event: React.KeyboardEvent<HTMLButtonElement>, optionValue: number) => {
		if (disabled) return;
		const currentIndex = enabledOptions.findIndex((option) => option.value === optionValue);
		if (currentIndex < 0) return;
		const direction = getTabNavigationDirection(event.key, 'horizontal');
		if (!direction) return;
		const nextIndex = getNextTabIndex(currentIndex, enabledOptions.length, direction);
		const nextOption = nextIndex == null ? null : enabledOptions[nextIndex];
		if (!nextOption) return;
		event.preventDefault();
		event.stopPropagation();
		onChange(nextOption.value);
		window.requestAnimationFrame(() => optionRefs.current.get(nextOption.value)?.focus());
	};
	return (
		<div className={styles.row} data-flx={`${dataFlx}.row`}>
			<div className={clsx(styles.text, disabled && styles.textDisabled)} data-flx={`${dataFlx}.text`}>
				<span id={labelId} className={styles.label} data-flx={`${dataFlx}.label`}>
					{label}
				</span>
				<span id={descriptionId} className={styles.description} data-flx={`${dataFlx}.description`}>
					{description}
				</span>
			</div>
			<div
				className={clsx(styles.choiceGroup, disabled && styles.choiceGroupDisabled)}
				role="radiogroup"
				aria-labelledby={labelId}
				aria-describedby={descriptionId}
				aria-disabled={disabled || undefined}
				data-flx={dataFlx}
			>
				{options.map((option) => {
					const isSelected = option.value === value;
					return (
						<FocusRing key={option.value} offset={-2} data-flx={`${dataFlx}.focus-ring`}>
							<button
								ref={(element) => {
									if (element) {
										optionRefs.current.set(option.value, element);
									} else {
										optionRefs.current.delete(option.value);
									}
								}}
								type="button"
								role="radio"
								aria-checked={isSelected}
								tabIndex={!disabled && option.value === focusedValue ? 0 : -1}
								disabled={disabled || option.disabled}
								className={clsx(styles.choiceButton, isSelected && styles.choiceButtonActive)}
								onClick={() => onChange(option.value)}
								onKeyDown={(event) => handleKeyDown(event, option.value)}
								data-flx={`${dataFlx}.button`}
							>
								{option.label}
							</button>
						</FocusRing>
					);
				})}
				{selectedIndex >= 0 && (
					<motion.div
						className={styles.choiceIndicator}
						layout={true}
						aria-hidden={true}
						transition={
							Accessibility.useReducedMotion
								? {duration: 0}
								: {
										type: 'spring',
										stiffness: 500,
										damping: 35,
									}
						}
						style={{
							width: `calc((100% - 0.375rem) / ${options.length})`,
							left: `calc(0.1875rem + (100% - 0.375rem) * ${selectedIndex} / ${options.length})`,
						}}
						data-flx={`${dataFlx}.indicator`}
					/>
				)}
			</div>
		</div>
	);
};

export const SensitiveContentTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const currentUser = Users.getCurrentUser();
	const isMatureContentAllowed = currentUser?.matureContentAllowed ?? false;
	const [friendDmFilter, setFriendDmFilter] = useState(UserSettings.sensitiveContentFriendDmFilter);
	const [nonFriendDmFilter, setNonFriendDmFilter] = useState(UserSettings.sensitiveContentNonFriendDmFilter);
	const [guildFilter, setGuildFilter] = useState(UserSettings.sensitiveContentGuildFilter);
	const [isSubmitting, setIsSubmitting] = useState(false);
	const hasUnsavedChanges =
		friendDmFilter !== UserSettings.sensitiveContentFriendDmFilter ||
		nonFriendDmFilter !== UserSettings.sensitiveContentNonFriendDmFilter ||
		(isMatureContentAllowed && guildFilter !== UserSettings.sensitiveContentGuildFilter);
	const handleReset = useCallback(() => {
		setFriendDmFilter(UserSettings.sensitiveContentFriendDmFilter);
		setNonFriendDmFilter(UserSettings.sensitiveContentNonFriendDmFilter);
		setGuildFilter(UserSettings.sensitiveContentGuildFilter);
	}, []);
	const handleSave = useCallback(async () => {
		setIsSubmitting(true);
		try {
			if (isMatureContentAllowed) {
				await UserSettingsCommands.update({
					sensitiveContentFriendDmFilter: friendDmFilter,
					sensitiveContentNonFriendDmFilter: nonFriendDmFilter,
					sensitiveContentGuildFilter: guildFilter,
				});
			} else {
				await UserSettingsCommands.update({
					sensitiveContentFriendDmFilter: friendDmFilter,
					sensitiveContentNonFriendDmFilter: nonFriendDmFilter,
				});
			}
		} finally {
			setIsSubmitting(false);
		}
	}, [isMatureContentAllowed, friendDmFilter, nonFriendDmFilter, guildFilter]);
	useEffect(() => {
		UnsavedChangesCommands.setUnsavedChanges(SENSITIVE_CONTENT_TAB_ID, hasUnsavedChanges);
	}, [hasUnsavedChanges]);
	useEffect(() => {
		UnsavedChangesCommands.setTabData(SENSITIVE_CONTENT_TAB_ID, {
			onReset: handleReset,
			onSave: handleSave,
			isSubmitting,
		});
	}, [handleReset, handleSave, isSubmitting]);
	useEffect(() => {
		return () => {
			UnsavedChangesCommands.clearUnsavedChanges(SENSITIVE_CONTENT_TAB_ID);
		};
	}, []);
	const filterOptions = useMemo(
		() => [
			{value: SensitiveMediaFilterLevel.SHOW, label: i18n._(SHOW_DESCRIPTOR)},
			{value: SensitiveMediaFilterLevel.BLUR, label: i18n._(BLUR_DESCRIPTOR)},
			{value: SensitiveMediaFilterLevel.BLOCK, label: i18n._(BLOCK_DESCRIPTOR)},
		],
		[i18n.locale],
	);
	const teenDmOptions = useMemo(
		() => [
			{value: SensitiveMediaFilterLevel.SHOW, label: i18n._(SHOW_DESCRIPTOR), disabled: true},
			{value: SensitiveMediaFilterLevel.BLUR, label: i18n._(BLUR_DESCRIPTOR)},
			{value: SensitiveMediaFilterLevel.BLOCK, label: i18n._(BLOCK_DESCRIPTOR)},
		],
		[i18n.locale],
	);
	const guildFilterOptions = useMemo(
		() => [
			{value: SensitiveMediaFilterLevel.SHOW, label: i18n._(SHOW_DESCRIPTOR)},
			{value: SensitiveMediaFilterLevel.BLUR, label: i18n._(BLUR_DESCRIPTOR)},
		],
		[i18n.locale],
	);
	return (
		<div
			className={styles.container}
			data-flx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.settings-tab-section"
		>
			<p
				className={styles.intro}
				data-flx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.intro"
			>
				{i18n._(INTRO_DESCRIPTOR)}
			</p>
			<div
				className={styles.rows}
				data-flx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.rows"
			>
				<SensitiveContentChoiceRow
					label={i18n._(DIRECT_MESSAGES_FROM_FRIENDS_DESCRIPTOR)}
					description={i18n._(FRIENDS_DESCRIPTION_DESCRIPTOR)}
					value={friendDmFilter}
					options={isMatureContentAllowed ? filterOptions : teenDmOptions}
					onChange={setFriendDmFilter}
					dataFlx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.select.set-friend-dm-filter"
					data-flx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.sensitive-content-choice-row.set-friend-dm-filter"
				/>
				<SensitiveContentChoiceRow
					label={i18n._(DIRECT_MESSAGES_FROM_OTHERS_DESCRIPTOR)}
					description={i18n._(OTHERS_DESCRIPTION_DESCRIPTOR)}
					value={nonFriendDmFilter}
					options={isMatureContentAllowed ? filterOptions : teenDmOptions}
					onChange={setNonFriendDmFilter}
					dataFlx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.select.set-non-friend-dm-filter"
					data-flx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.sensitive-content-choice-row.set-non-friend-dm-filter"
				/>
				<SensitiveContentChoiceRow
					label={i18n._(MESSAGES_IN_COMMUNITY_CHANNELS_DESCRIPTOR)}
					description={i18n._(COMMUNITY_DESCRIPTION_DESCRIPTOR)}
					value={guildFilter}
					options={guildFilterOptions}
					onChange={setGuildFilter}
					disabled={!isMatureContentAllowed}
					dataFlx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.select.set-guild-filter"
					data-flx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.sensitive-content-choice-row.set-guild-filter"
				/>
			</div>
		</div>
	);
});
