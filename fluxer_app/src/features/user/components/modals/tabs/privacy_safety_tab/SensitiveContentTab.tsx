// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
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
const SENSITIVE_CONTENT_TAB_ID = 'privacy_safety';

interface SensitiveContentOption {
	value: number;
	label: string;
}

interface SensitiveContentChoiceRowProps {
	label: string;
	value: number;
	options: ReadonlyArray<SensitiveContentOption>;
	onChange: (value: number) => void;
	disabled?: boolean;
	dataFlx: string;
}

const SensitiveContentChoiceRow: React.FC<SensitiveContentChoiceRowProps> = ({
	label,
	value,
	options,
	onChange,
	disabled,
	dataFlx,
}) => {
	const labelId = useId();
	const optionRefs = useRef(new Map<number, HTMLButtonElement>());
	const selectedIndex = options.findIndex((option) => option.value === value);
	const focusedIndex = selectedIndex >= 0 ? selectedIndex : 0;
	const handleKeyDown = (event: React.KeyboardEvent<HTMLButtonElement>, optionValue: number) => {
		if (disabled) return;
		const currentIndex = options.findIndex((option) => option.value === optionValue);
		if (currentIndex < 0) return;
		const direction = getTabNavigationDirection(event.key, 'horizontal');
		if (!direction) return;
		const nextIndex = getNextTabIndex(currentIndex, options.length, direction);
		const nextOption = nextIndex == null ? null : options[nextIndex];
		if (!nextOption) return;
		event.preventDefault();
		event.stopPropagation();
		onChange(nextOption.value);
		window.requestAnimationFrame(() => optionRefs.current.get(nextOption.value)?.focus());
	};
	return (
		<div className={styles.row} data-flx={`${dataFlx}.row`}>
			<span id={labelId} className={clsx(styles.label, disabled && styles.labelDisabled)} data-flx={`${dataFlx}.label`}>
				{label}
			</span>
			<div className={styles.choiceWrap} data-flx={`${dataFlx}.choice-wrap`}>
				<div
					className={clsx(styles.choiceGroup, disabled && styles.choiceGroupDisabled)}
					role="radiogroup"
					aria-labelledby={labelId}
					aria-disabled={disabled || undefined}
					data-flx={dataFlx}
				>
					{options.map((option, index) => {
						const isSelected = option.value === value;
						return (
							<button
								key={option.value}
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
								tabIndex={!disabled && index === focusedIndex ? 0 : -1}
								disabled={disabled}
								className={clsx(styles.choiceButton, isSelected && styles.choiceButtonActive)}
								onClick={() => onChange(option.value)}
								onKeyDown={(event) => handleKeyDown(event, option.value)}
								data-flx={`${dataFlx}.button`}
							>
								{option.label}
							</button>
						);
					})}
					{selectedIndex >= 0 && (
						<motion.div
							className={styles.choiceIndicator}
							layout={true}
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
	const hasUnsavedChanges = isMatureContentAllowed
		? friendDmFilter !== UserSettings.sensitiveContentFriendDmFilter ||
			nonFriendDmFilter !== UserSettings.sensitiveContentNonFriendDmFilter ||
			guildFilter !== UserSettings.sensitiveContentGuildFilter
		: friendDmFilter !== UserSettings.sensitiveContentFriendDmFilter;
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
	const teenFriendDmOptions = useMemo(
		() => [
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
			<SensitiveContentChoiceRow
				label={i18n._(DIRECT_MESSAGES_FROM_FRIENDS_DESCRIPTOR)}
				value={friendDmFilter}
				options={isMatureContentAllowed ? filterOptions : teenFriendDmOptions}
				onChange={setFriendDmFilter}
				dataFlx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.select.set-friend-dm-filter"
				data-flx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.sensitive-content-choice-row.set-friend-dm-filter"
			/>
			<SensitiveContentChoiceRow
				label={i18n._(DIRECT_MESSAGES_FROM_OTHERS_DESCRIPTOR)}
				value={nonFriendDmFilter}
				options={filterOptions}
				onChange={setNonFriendDmFilter}
				disabled={!isMatureContentAllowed}
				dataFlx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.select.set-non-friend-dm-filter"
				data-flx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.sensitive-content-choice-row.set-non-friend-dm-filter"
			/>
			<SensitiveContentChoiceRow
				label={i18n._(MESSAGES_IN_COMMUNITY_CHANNELS_DESCRIPTOR)}
				value={guildFilter}
				options={guildFilterOptions}
				onChange={setGuildFilter}
				disabled={!isMatureContentAllowed}
				dataFlx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.select.set-guild-filter"
				data-flx="user.privacy-safety-tab.sensitive-content-tab.sensitive-content-tab-content.sensitive-content-choice-row.set-guild-filter"
			/>
		</div>
	);
});
