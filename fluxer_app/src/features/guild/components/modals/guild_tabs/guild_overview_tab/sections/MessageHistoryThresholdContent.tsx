// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {EVERYONE_MENTION} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/GuildOverviewTab.module.css';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import {DateTimePickerField} from '@app/features/ui/components/form/DateTimePickerField';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {formatGuildSettingsPath} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {AnimatePresence, motion} from 'framer-motion';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';
import type {ControllerRenderProps, FieldValues, Path, UseFormReturn} from 'react-hook-form';
import {Controller} from 'react-hook-form';

const HOW_THIS_WORKS_DESCRIPTOR = msg({
	message: 'How this works',
	comment: 'Short label in the message history threshold content. Keep it concise.',
});
const THRESHOLD_DATE_CANNOT_BE_BEFORE_COMMUNITY_CREATION_DESCRIPTOR = msg({
	message: 'Threshold date cannot be before community creation',
	comment: 'Error message in the message history threshold content.',
});
const THRESHOLD_DATE_CANNOT_BE_IN_THE_FUTURE_DESCRIPTOR = msg({
	message: 'Threshold date cannot be in the future',
	comment: 'Error message in the message history threshold content.',
});
const ENABLE_MESSAGE_HISTORY_THRESHOLD_DESCRIPTOR = msg({
	message: 'Enable message history threshold',
	comment: 'Button or menu action label in the message history threshold content. Keep it concise.',
});
const ALLOW_MEMBERS_WITHOUT_THE_PERMISSION_TO_SEE_MESSAGES_DESCRIPTOR = msg({
	message:
		'Allow members without the {readMessageHistoryPermissionLabel} permission to see messages sent after a specific date.',
	comment: 'Label in the message history threshold content. Keep the tone plain and specific.',
});
const THRESHOLD_DATE_DESCRIPTOR = msg({
	message: 'Threshold date',
	comment: 'Short label in the message history threshold content. Keep it concise.',
});
const MEMBERS_WITHOUT_THE_PERMISSION_CAN_VIEW_MESSAGES_SENT_DESCRIPTOR = msg({
	message: 'Members without the {readMessageHistoryPermissionLabel} permission can view messages sent after this date.',
	comment:
		'Description text in the message history threshold content. Preserve {readMessageHistoryPermissionLabel}; it is inserted by code. Keep the tone plain and specific.',
});

export interface MessageHistoryThresholdFormValues {
	message_history_cutoff: string | null;
}

export const MessageHistoryThresholdDescription: React.FC = () => {
	const {i18n} = useLingui();
	const rolesSettingsPath = formatGuildSettingsPath(i18n, 'roles');
	const administratorPermissionLabel = formatPermissionLabel(i18n, Permissions.ADMINISTRATOR);
	const readMessageHistoryPermissionLabel = formatPermissionLabel(i18n, Permissions.READ_MESSAGE_HISTORY);
	return (
		<div
			className={styles.messageHistoryDescription}
			data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.message-history-description"
		>
			<p data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.p">
				<Trans>
					When a custom threshold date is not set, members without the{' '}
					<strong data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.strong">
						{readMessageHistoryPermissionLabel}
					</strong>{' '}
					permission cannot view any historical messages. They'll only see messages in real time as they arrive, and
					they'll disappear when they restart their clients.
				</Trans>
			</p>
			<p data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.p--2">
				<Trans>
					If you'd like these members to access historical messages sent after a specific date, enable this feature and
					choose a threshold below.
				</Trans>
			</p>
			<ul
				className={styles.messageHistoryList}
				data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.message-history-list"
			>
				<li data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.li">
					<Trans>
						You can remove the{' '}
						<strong data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.strong--2">
							{readMessageHistoryPermissionLabel}
						</strong>{' '}
						permission from the{' '}
						<strong data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.strong--3">
							{EVERYONE_MENTION}
						</strong>{' '}
						role in{' '}
						<strong data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.strong--4">
							{rolesSettingsPath}
						</strong>{' '}
						and grant it to one or more trusted roles instead.
					</Trans>
				</li>
				<li data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.li--2">
					<Trans>
						Community owners and users with the{' '}
						<strong data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.strong--5">
							{administratorPermissionLabel}
						</strong>{' '}
						permission always have all permissions, including{' '}
						<strong data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.strong--6">
							{readMessageHistoryPermissionLabel}
						</strong>
						, so they aren't affected by this setting.
					</Trans>
				</li>
				<li data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.li--3">
					<Trans>
						You can also create per-category or per-channel permission overrides to grant{' '}
						<strong data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-description.strong--7">
							{EVERYONE_MENTION}
						</strong>{' '}
						full access to rules or announcement channels, for example.
					</Trans>
				</li>
			</ul>
		</div>
	);
};
export const MessageHistoryThresholdAccordion: React.FC = () => {
	const {i18n} = useLingui();
	const [isOpen, setIsOpen] = useState(false);
	const toggle = useCallback(() => setIsOpen((current) => !current), []);
	return (
		<div
			className={styles.messageHistoryAccordion}
			data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-accordion.message-history-accordion"
		>
			<button
				type="button"
				className={styles.messageHistoryAccordionToggle}
				onClick={toggle}
				aria-expanded={isOpen}
				data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-accordion.message-history-accordion-toggle.button"
			>
				<span
					className={styles.messageHistoryAccordionTitle}
					data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-accordion.message-history-accordion-title"
				>
					{i18n._(HOW_THIS_WORKS_DESCRIPTOR)}
				</span>
				<motion.span
					className={styles.messageHistoryAccordionChevron}
					aria-hidden
					animate={{rotate: isOpen ? 45 : -45}}
					transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2, ease: 'easeOut'}}
					data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-accordion.message-history-accordion-chevron"
				/>
			</button>
			<AnimatePresence
				initial={false}
				data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-accordion.animate-presence"
			>
				{isOpen && (
					<motion.div
						className={styles.messageHistoryAccordionContent}
						initial={Accessibility.useReducedMotion ? {height: 'auto', opacity: 1} : {height: 0, opacity: 0}}
						animate={{height: 'auto', opacity: 1}}
						exit={Accessibility.useReducedMotion ? {height: 'auto', opacity: 1} : {height: 0, opacity: 0}}
						transition={{duration: Accessibility.useReducedMotion ? 0 : 0.25, ease: 'easeOut'}}
						data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-accordion.message-history-accordion-content"
					>
						<MessageHistoryThresholdDescription data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-accordion.message-history-threshold-description" />
					</motion.div>
				)}
			</AnimatePresence>
		</div>
	);
};

export function buildMessageHistoryThresholdValidator(
	i18n: I18n,
	guildCreatedAt: Date,
): (value: string | null) => true | string {
	return (value) => {
		if (value === null) {
			return true;
		}
		const cutoffTimestamp = new Date(value).getTime();
		if (cutoffTimestamp < guildCreatedAt.getTime()) {
			return i18n._(THRESHOLD_DATE_CANNOT_BE_BEFORE_COMMUNITY_CREATION_DESCRIPTOR);
		}
		if (cutoffTimestamp > Date.now()) {
			return i18n._(THRESHOLD_DATE_CANNOT_BE_IN_THE_FUTURE_DESCRIPTOR);
		}
		return true;
	};
}

interface MessageHistoryThresholdFieldProps<T extends FieldValues> {
	form: UseFormReturn<T>;
	name: Path<T>;
	canManageGuild: boolean;
	guildCreatedAt: Date;
	maxDate: Date;
}

export function MessageHistoryThresholdField<T extends FieldValues>({
	form,
	name,
	canManageGuild,
	guildCreatedAt,
	maxDate,
}: MessageHistoryThresholdFieldProps<T>) {
	const {i18n} = useLingui();
	return (
		<Controller
			name={name}
			control={form.control}
			rules={{
				validate: buildMessageHistoryThresholdValidator(i18n, guildCreatedAt),
			}}
			render={({field, fieldState}) => (
				<MessageHistoryThresholdPicker
					field={field}
					error={fieldState.error?.message}
					canManageGuild={canManageGuild}
					guildCreatedAt={guildCreatedAt}
					maxDate={maxDate}
					data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-field.message-history-threshold-picker"
				/>
			)}
			data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-field.controller"
		/>
	);
}

interface MessageHistoryThresholdPickerProps<T extends FieldValues> {
	field: ControllerRenderProps<T, Path<T>>;
	error?: string;
	canManageGuild: boolean;
	guildCreatedAt: Date;
	maxDate: Date;
}

function MessageHistoryThresholdPicker<T extends FieldValues>({
	field,
	error,
	canManageGuild,
	guildCreatedAt,
	maxDate,
}: MessageHistoryThresholdPickerProps<T>) {
	const {i18n} = useLingui();
	const readMessageHistoryPermissionLabel = formatPermissionLabel(i18n, Permissions.READ_MESSAGE_HISTORY);
	const isEnabled = field.value != null;
	const handleToggle = useCallback(
		(enabled: boolean) => {
			if (enabled) {
				field.onChange(guildCreatedAt.toISOString());
			} else {
				field.onChange(null);
			}
		},
		[field, guildCreatedAt],
	);
	const handleDateChange = useCallback(
		(date: Date | null) => {
			if (date) {
				field.onChange(date.toISOString());
			}
		},
		[field],
	);
	const dateValue = useMemo(() => (typeof field.value === 'string' ? new Date(field.value) : null), [field.value]);
	return (
		<div data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-picker.div">
			<Switch
				label={i18n._(ENABLE_MESSAGE_HISTORY_THRESHOLD_DESCRIPTOR)}
				description={i18n._(ALLOW_MEMBERS_WITHOUT_THE_PERMISSION_TO_SEE_MESSAGES_DESCRIPTOR, {
					readMessageHistoryPermissionLabel,
				})}
				value={isEnabled}
				onChange={handleToggle}
				disabled={!canManageGuild}
				data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-picker.switch.toggle"
			/>
			{isEnabled && (
				<DateTimePickerField
					className={styles.dateTimePickerField}
					label={i18n._(THRESHOLD_DATE_DESCRIPTOR)}
					description={i18n._(MEMBERS_WITHOUT_THE_PERMISSION_CAN_VIEW_MESSAGES_SENT_DESCRIPTOR, {
						readMessageHistoryPermissionLabel,
					})}
					value={dateValue}
					onChange={handleDateChange}
					minDate={guildCreatedAt}
					maxDate={maxDate}
					disabled={!canManageGuild}
					error={error}
					data-flx="guild.guild-tabs.guild-overview-tab.message-history-threshold-content.message-history-threshold-picker.date-time-picker-field.date-change"
				/>
			)}
		</div>
	);
}
