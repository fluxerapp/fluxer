// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import {
	CANCEL_DESCRIPTOR,
	CONTINUE_DESCRIPTOR,
	GO_BACK_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getCachedDateTimeFormat} from '@app/features/i18n/utils/IntlCache';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {DateTimePickerField} from '@app/features/ui/components/form/DateTimePickerField';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {SwitchGroup, SwitchGroupItem} from '@app/features/ui/components/SwitchGroup';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import type {BulkDeleteMyMessagesFilter} from '@app/features/user/commands/UserCommands';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import styles from '@app/features/user/components/modals/tabs/privacy_safety_tab/data_request_modal/DataRequestModal.module.css';
import * as FormUtils from '@app/lib/forms';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';

export type DataRequestVariant = 'export' | 'delete';
type ScopeValue = 'everything' | 'custom' | 'selected' | 'inaccessible_only';
type DateMode = 'all_time' | 'custom';
type GuildFilterMode = 'exclude' | 'include_only';
type Step = 'scope' | 'kinds' | 'communities' | 'when' | 'confirm';

const STEP_ORDER: ReadonlyArray<Step> = ['scope', 'kinds', 'communities', 'when', 'confirm'];
const EXPORT_TITLE_DESCRIPTOR = msg({
	message: 'Export my data',
	comment: 'Privacy > Data export: modal title for the redesigned data-export dialog with scope and date filters.',
});
const DELETE_TITLE_DESCRIPTOR = msg({
	message: 'Delete my messages',
	comment:
		'Privacy > Data deletion: modal title for the redesigned message-deletion dialog with scope and date filters.',
});
const EXPORT_SUCCESS_DESCRIPTOR = msg({
	message: "We'll process this as soon as possible. You'll get an email when your archive is ready.",
	comment: 'Success toast shown after a filtered data export job is queued.',
});
const DELETE_SUCCESS_DESCRIPTOR = msg({
	message: "We'll process this as soon as possible. You'll get a DM from us when it's done.",
	comment: 'Success toast shown after a bulk message deletion job is queued.',
});
const EXPORT_SCOPE_EVERYTHING_LABEL_DESCRIPTOR = msg({
	message: 'Everything',
	comment: 'Scope option (radio) that exports the user every message and metadata. Default.',
});
const EXPORT_SCOPE_EVERYTHING_DESC_DESCRIPTOR = msg({
	message: 'Export every message you have ever sent, plus all of your account settings, memberships, and metadata.',
	comment: 'Description for the Everything export option.',
});
const EXPORT_SCOPE_CUSTOM_LABEL_DESCRIPTOR = msg({
	message: 'Custom selection',
	comment: 'Scope option (radio) that lets the user filter what to export.',
});
const EXPORT_SCOPE_CUSTOM_DESC_DESCRIPTOR = msg({
	message: 'Choose which conversation kinds, communities, and time window to include in the archive.',
	comment: 'Description for the Custom export option.',
});
const DELETE_SCOPE_SELECTED_LABEL_DESCRIPTOR = msg({
	message: 'Choose what to include',
	comment: 'Scope option (radio) that lets the user pick conversation kinds and exclude specific communities.',
});
const DELETE_SCOPE_SELECTED_DESC_DESCRIPTOR = msg({
	message: 'Pick which kinds of conversations to clean up.',
	comment: 'Description for the Choose what to include scope option.',
});
const DELETE_SCOPE_INACCESSIBLE_LABEL_DESCRIPTOR = msg({
	message: "Only places I can't access anymore",
	comment:
		'Scope option (radio) that only deletes messages in communities and group DMs the user has been removed from or left.',
});
const DELETE_SCOPE_INACCESSIBLE_DESC_DESCRIPTOR = msg({
	message: 'Only delete messages from communities and group DMs you have left or been removed from.',
	comment: 'Description for the inaccessible-only scope option.',
});
const SCOPE_STEP_TITLE_DESCRIPTOR = msg({
	message: 'What to include',
	comment: 'Heading of the first step in the data-request carousel modal asking the user to pick the scope.',
});
const KINDS_STEP_TITLE_DESCRIPTOR = msg({
	message: 'Which conversations',
	comment: 'Heading of the kinds step in the data-request carousel asking which kinds of conversations to include.',
});
const KINDS_STEP_BODY_DESCRIPTOR = msg({
	message: 'Toggle the kinds of conversations you want included.',
	comment: 'Subhead under the kinds step in the data-request carousel modal.',
});
const COMMUNITIES_STEP_TITLE_DESCRIPTOR = msg({
	message: 'Which communities',
	comment: 'Heading of the communities step in the data-request carousel modal.',
});
const WHEN_STEP_TITLE_DESCRIPTOR = msg({
	message: 'Time range',
	comment: 'Heading of the time-range step in the data-request carousel modal.',
});
const CONFIRM_STEP_TITLE_DESCRIPTOR = msg({
	message: 'Review and confirm',
	comment: 'Heading of the final confirm step in the data-request carousel modal showing a summary.',
});
const EXPORT_CONFIRM_EVERYTHING_DESCRIPTOR = msg({
	message:
		"We'll build a downloadable archive of every message you have ever sent and email you when it's ready. The download link in that email expires after 7 days.",
	comment: 'Confirm-step copy in the data-export modal when scope is everything.',
});
const EXPORT_CONFIRM_CUSTOM_DESCRIPTOR = msg({
	message:
		"We'll build a downloadable archive that matches the filters below and email you when it's ready. The download link in that email expires after 7 days.",
	comment: 'Confirm-step copy in the data-export modal when scope is custom.',
});
const DELETE_CONFIRM_DESCRIPTOR = msg({
	message: 'Permanently delete the messages that match the filters below. This cannot be undone.',
	comment: 'Confirm-step copy in the data-deletion modal explaining the destructive operation.',
});
const DELETE_DANGER_NOTICE_DESCRIPTOR = msg({
	message: 'There is no recovery once this starts. We will DM you when it finishes.',
	comment: 'Destructive notice on the confirm step of the data-deletion carousel modal.',
});
const KIND_DMS_LABEL_DESCRIPTOR = msg({
	message: 'Open DMs',
	comment:
		'Switch label for including 1:1 direct messages the user still has open in their DM list. Independent companion to the Closed DMs switch.',
});
const KIND_DMS_CLOSED_LABEL_DESCRIPTOR = msg({
	message: 'Closed DMs',
	comment:
		'Switch label for including 1:1 direct messages the user has previously closed. Independent companion to the Open DMs switch.',
});
const KIND_GROUP_DMS_LABEL_DESCRIPTOR = msg({
	message: 'Group DMs',
	comment: 'Switch label for including group DMs (3+ participants) in a data export / deletion scope.',
});
const KIND_COMMUNITIES_LABEL_DESCRIPTOR = msg({
	message: 'Communities',
	comment: 'Switch label for including community (guild) channels in a data export / deletion scope.',
});
const GUILD_FILTER_MODE_LABEL_DESCRIPTOR = msg({
	message: 'Community filter',
	comment: 'Form label for the dropdown that switches the community filter between exclude and include-only modes.',
});
const GUILD_FILTER_MODE_EXCLUDE_DESCRIPTOR = msg({
	message: 'Include all except selected',
	comment: 'Dropdown option that switches the community filter into exclude mode.',
});
const GUILD_FILTER_MODE_INCLUDE_DESCRIPTOR = msg({
	message: 'Only the selected ones',
	comment: 'Dropdown option that switches the community filter into include-only mode.',
});
const COMMUNITIES_EMPTY_DESCRIPTOR = msg({
	message: "You aren't in any communities right now.",
	comment: 'Placeholder shown when the user has no communities to filter.',
});
const DATE_MODE_LABEL_DESCRIPTOR = msg({
	message: 'Time range',
	comment: 'Form label for the dropdown that picks between all-time and a custom date range.',
});
const DATE_MODE_ALL_TIME_DESCRIPTOR = msg({
	message: 'All time',
	comment: 'Dropdown option that applies the action to every message.',
});
const DATE_MODE_CUSTOM_DESCRIPTOR = msg({
	message: 'Custom range',
	comment: 'Dropdown option that lets the user pick a start and end date.',
});
const START_DATE_DESCRIPTOR = msg({
	message: 'Start date',
	comment: 'Label for the start of a date range (inclusive). Leave blank for no lower bound.',
});
const END_DATE_DESCRIPTOR = msg({
	message: 'End date',
	comment: 'Label for the end of a date range (exclusive). Leave blank for no upper bound.',
});
const DATE_HELPER_DESCRIPTOR = msg({
	message: 'Leave either field blank to leave that end of the window unbounded.',
	comment: 'Helper text under the custom date range pickers explaining optionality.',
});
const NEED_INCLUSION_DESCRIPTOR = msg({
	message: 'Pick at least one kind of conversation to include.',
	comment: 'Validation error when the user advances with all inclusion toggles off in the custom scope.',
});
const DATE_RANGE_ERROR_DESCRIPTOR = msg({
	message: 'Start date must be earlier than end date.',
	comment: 'Validation error when start_date is on or after end_date.',
});
const EXPORT_SUBMIT_DESCRIPTOR = msg({
	message: 'Request export',
	comment: 'Footer submit button label on the confirm step of the data-export modal.',
});
const DELETE_SUBMIT_DESCRIPTOR = msg({
	message: 'Delete messages',
	comment: 'Footer submit button label on the confirm step of the data-deletion modal.',
});
const SUMMARY_SCOPE_LABEL_DESCRIPTOR = msg({
	message: 'Scope',
	comment: 'Summary row label on the confirm step showing the user-picked scope.',
});
const SUMMARY_CONVERSATIONS_LABEL_DESCRIPTOR = msg({
	message: 'Conversations',
	comment: 'Summary row label on the confirm step showing the user-picked conversation kinds.',
});
const SUMMARY_COMMUNITIES_LABEL_DESCRIPTOR = msg({
	message: 'Communities',
	comment: 'Summary row label on the confirm step showing the community filter selection.',
});
const SUMMARY_TIME_RANGE_LABEL_DESCRIPTOR = msg({
	message: 'Time range',
	comment: 'Summary row label on the confirm step showing the chosen time range.',
});
const SUMMARY_NONE_DESCRIPTOR = msg({
	message: 'None',
	comment: 'Summary row value on the confirm step when no items in the category were selected.',
});
const SUMMARY_ALL_TIME_DESCRIPTOR = msg({
	message: 'All time',
	comment: 'Summary row value on the confirm step indicating no date filter is applied.',
});
const SUMMARY_FROM_DESCRIPTOR = msg({
	message: 'From {start}',
	comment: 'Summary row value on the confirm step when only a start date is set. {start} is a localized date.',
});
const SUMMARY_UNTIL_DESCRIPTOR = msg({
	message: 'Until {end}',
	comment: 'Summary row value on the confirm step when only an end date is set. {end} is a localized date.',
});
const SUMMARY_BETWEEN_DESCRIPTOR = msg({
	message: '{start} – {end}',
	comment: 'Summary row value when both a start and end date are set. Both values are localized dates.',
});
const SUMMARY_GUILD_EXCLUDE_DESCRIPTOR = msg({
	message: 'All except {count, plural, one {# community} other {# communities}}',
	comment: 'Summary row value on the confirm step when the community filter is in exclude mode.',
});
const SUMMARY_GUILD_INCLUDE_DESCRIPTOR = msg({
	message: 'Only {count, plural, one {# community} other {# communities}}',
	comment: 'Summary row value on the confirm step when the community filter is in include-only mode.',
});
const SUMMARY_DMS_OPEN_ONLY_DESCRIPTOR = msg({
	message: 'Open direct messages',
	comment: 'Summary chip on the confirm step indicating only currently-open 1:1 DMs are included.',
});
const SUMMARY_DMS_CLOSED_ONLY_DESCRIPTOR = msg({
	message: 'Closed direct messages',
	comment: 'Summary chip on the confirm step indicating only previously-closed 1:1 DMs are included.',
});
const SUMMARY_DMS_WITH_CLOSED_DESCRIPTOR = msg({
	message: 'Direct messages (open and closed)',
	comment: 'Summary chip on the confirm step indicating both open and closed 1:1 DMs are included.',
});
const SUMMARY_GROUP_DMS_DESCRIPTOR = msg({
	message: 'Group DMs',
	comment: 'Summary chip on the confirm step indicating that group DMs are included.',
});
const SUMMARY_COMMUNITIES_INCLUDED_DESCRIPTOR = msg({
	message: 'Communities',
	comment: 'Summary chip on the confirm step indicating that community channels are included.',
});
const EXPORT_TAB_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Build a downloadable archive of your account data, including messages and attachment URLs. Most people want everything, but you can narrow the scope below.',
	comment: 'Description shown on the settings page above the Export my data button.',
});
const DELETE_TAB_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Permanently remove messages you have sent across DMs, group DMs, and communities. The work runs in the background, and you will get a DM when it finishes.',
	comment: 'Description shown on the settings page above the Delete my messages button.',
});

interface ScopeOptionDescriptors {
	value: ScopeValue;
	labelDescriptor: MessageDescriptor;
	descDescriptor: MessageDescriptor;
}

interface VariantConfig {
	titleDescriptor: MessageDescriptor;
	defaultScope: ScopeValue;
	customScope: ScopeValue;
	scopeOptionDescriptors: ReadonlyArray<ScopeOptionDescriptors>;
	submitLabelDescriptor: MessageDescriptor;
	submitVariant: 'primary' | 'danger';
	successToastDescriptor: MessageDescriptor;
	confirmSimpleDescriptor: MessageDescriptor;
	confirmCustomDescriptor: MessageDescriptor;
	isDelete: boolean;
}

function getVariantConfig(variant: DataRequestVariant): VariantConfig {
	if (variant === 'export') {
		return {
			titleDescriptor: EXPORT_TITLE_DESCRIPTOR,
			defaultScope: 'everything',
			customScope: 'custom',
			scopeOptionDescriptors: [
				{
					value: 'everything',
					labelDescriptor: EXPORT_SCOPE_EVERYTHING_LABEL_DESCRIPTOR,
					descDescriptor: EXPORT_SCOPE_EVERYTHING_DESC_DESCRIPTOR,
				},
				{
					value: 'custom',
					labelDescriptor: EXPORT_SCOPE_CUSTOM_LABEL_DESCRIPTOR,
					descDescriptor: EXPORT_SCOPE_CUSTOM_DESC_DESCRIPTOR,
				},
			],
			submitLabelDescriptor: EXPORT_SUBMIT_DESCRIPTOR,
			submitVariant: 'primary',
			successToastDescriptor: EXPORT_SUCCESS_DESCRIPTOR,
			confirmSimpleDescriptor: EXPORT_CONFIRM_EVERYTHING_DESCRIPTOR,
			confirmCustomDescriptor: EXPORT_CONFIRM_CUSTOM_DESCRIPTOR,
			isDelete: false,
		};
	}
	return {
		titleDescriptor: DELETE_TITLE_DESCRIPTOR,
		defaultScope: 'selected',
		customScope: 'selected',
		scopeOptionDescriptors: [
			{
				value: 'selected',
				labelDescriptor: DELETE_SCOPE_SELECTED_LABEL_DESCRIPTOR,
				descDescriptor: DELETE_SCOPE_SELECTED_DESC_DESCRIPTOR,
			},
			{
				value: 'inaccessible_only',
				labelDescriptor: DELETE_SCOPE_INACCESSIBLE_LABEL_DESCRIPTOR,
				descDescriptor: DELETE_SCOPE_INACCESSIBLE_DESC_DESCRIPTOR,
			},
		],
		submitLabelDescriptor: DELETE_SUBMIT_DESCRIPTOR,
		submitVariant: 'danger',
		successToastDescriptor: DELETE_SUCCESS_DESCRIPTOR,
		confirmSimpleDescriptor: DELETE_CONFIRM_DESCRIPTOR,
		confirmCustomDescriptor: DELETE_CONFIRM_DESCRIPTOR,
		isDelete: true,
	};
}

function formatDateForSummary(date: Date, locale: string): string {
	return getCachedDateTimeFormat(locale, {year: 'numeric', month: 'short', day: 'numeric'}).format(date);
}

interface DataRequestModalProps {
	variant: DataRequestVariant;
}

export const DataRequestModal: React.FC<DataRequestModalProps> = observer(({variant}) => {
	const {i18n} = useLingui();
	const config = useMemo(() => getVariantConfig(variant), [variant]);
	const scopeOptions = useMemo<ReadonlyArray<RadioOption<ScopeValue>>>(
		() =>
			config.scopeOptionDescriptors.map((opt) => ({
				value: opt.value,
				name: i18n._(opt.labelDescriptor),
				desc: i18n._(opt.descDescriptor),
			})),
		[config, i18n.locale],
	);
	const [step, setStep] = useState<Step>('scope');
	const [scope, setScope] = useState<ScopeValue>(config.defaultScope);
	const [includeDms, setIncludeDms] = useState(true);
	const [includeDmsClosed, setIncludeDmsClosed] = useState(true);
	const [includeGroupDms, setIncludeGroupDms] = useState(true);
	const [includeGuilds, setIncludeGuilds] = useState(true);
	const [guildFilterMode, setGuildFilterMode] = useState<GuildFilterMode>('exclude');
	const [excludedGuildIds, setExcludedGuildIds] = useState<ReadonlySet<string>>(new Set());
	const [includedGuildIds, setIncludedGuildIds] = useState<ReadonlySet<string>>(new Set());
	const [dateMode, setDateMode] = useState<DateMode>('all_time');
	const [startDate, setStartDate] = useState<Date | null>(null);
	const [endDate, setEndDate] = useState<Date | null>(null);
	const [isSubmitting, setIsSubmitting] = useState(false);
	const isCustomScope = scope === config.customScope;
	const showKinds = isCustomScope;
	const showCommunities = isCustomScope && includeGuilds;
	const showWhen = isCustomScope || config.isDelete;
	const visibleSteps = useMemo<ReadonlyArray<Step>>(() => {
		const steps: Array<Step> = ['scope'];
		if (showKinds) steps.push('kinds');
		if (showCommunities) steps.push('communities');
		if (showWhen) steps.push('when');
		steps.push('confirm');
		return steps;
	}, [showKinds, showCommunities, showWhen]);
	const currentStepIndex = Math.max(0, visibleSteps.indexOf(step));
	const isLastStep = step === 'confirm';
	const guildList = useMemo(
		() =>
			Guilds.getGuilds()
				.slice()
				.sort((a, b) => a.name.localeCompare(b.name)),
		[],
	);
	const dateRangeError =
		dateMode === 'custom' && startDate && endDate && startDate.getTime() >= endDate.getTime()
			? i18n._(DATE_RANGE_ERROR_DESCRIPTOR)
			: null;
	const needsInclusion =
		isCustomScope && !includeDms && !includeDmsClosed && !includeGroupDms && !includeGuilds
			? i18n._(NEED_INCLUSION_DESCRIPTOR)
			: null;
	const canContinue = (() => {
		switch (step) {
			case 'scope':
				return true;
			case 'kinds':
				return !needsInclusion;
			case 'communities':
				return true;
			case 'when':
				return !dateRangeError;
			case 'confirm':
				return !isSubmitting;
		}
	})();
	const closeModal = useCallback(() => ModalCommands.pop(), []);
	const goBack = useCallback(() => {
		const i = visibleSteps.indexOf(step);
		if (i > 0) {
			setStep(visibleSteps[i - 1]);
		}
	}, [step, visibleSteps]);
	const goNext = useCallback(() => {
		const i = visibleSteps.indexOf(step);
		if (i >= 0 && i < visibleSteps.length - 1) {
			setStep(visibleSteps[i + 1]);
		}
	}, [step, visibleSteps]);
	const toggleExcludedGuild = useCallback((guildId: string, value: boolean) => {
		setExcludedGuildIds((prev) => {
			const next = new Set(prev);
			if (value) next.add(guildId);
			else next.delete(guildId);
			return next;
		});
	}, []);
	const toggleIncludedGuild = useCallback((guildId: string, value: boolean) => {
		setIncludedGuildIds((prev) => {
			const next = new Set(prev);
			if (value) next.add(guildId);
			else next.delete(guildId);
			return next;
		});
	}, []);
	const buildFilter = useCallback(
		(filterScope: BulkDeleteMyMessagesFilter['scope']): BulkDeleteMyMessagesFilter => {
			const guildsActive = filterScope === 'selected' && includeGuilds;
			return {
				scope: filterScope,
				include_dms: filterScope === 'selected' ? includeDms : false,
				include_dms_closed: filterScope === 'selected' ? includeDmsClosed : false,
				include_group_dms: filterScope === 'selected' ? includeGroupDms : false,
				include_guilds: filterScope === 'selected' ? includeGuilds : true,
				guild_filter_mode: guildFilterMode,
				excluded_guild_ids: guildsActive && guildFilterMode === 'exclude' ? Array.from(excludedGuildIds) : [],
				included_guild_ids: guildsActive && guildFilterMode === 'include_only' ? Array.from(includedGuildIds) : [],
				start_date: dateMode === 'custom' && startDate ? startDate.toISOString() : null,
				end_date: dateMode === 'custom' && endDate ? endDate.toISOString() : null,
			};
		},
		[
			includeDms,
			includeDmsClosed,
			includeGroupDms,
			includeGuilds,
			guildFilterMode,
			excludedGuildIds,
			includedGuildIds,
			dateMode,
			startDate,
			endDate,
		],
	);
	const handleSubmit = useCallback(async () => {
		if (isSubmitting) return;
		setIsSubmitting(true);
		try {
			if (variant === 'export') {
				if (scope === 'everything') {
					await UserCommands.requestDataHarvest();
				} else {
					await UserCommands.requestFilteredDataHarvest(buildFilter('selected'));
				}
			} else {
				const filterScope: BulkDeleteMyMessagesFilter['scope'] =
					scope === 'inaccessible_only' ? 'inaccessible_only' : 'selected';
				await UserCommands.bulkDeleteMyMessages(buildFilter(filterScope));
			}
			ToastCommands.createToast({type: 'success', children: i18n._(config.successToastDescriptor)});
			closeModal();
		} catch (error) {
			FormUtils.pushApiErrorModal(i18n, error);
		} finally {
			setIsSubmitting(false);
		}
	}, [variant, scope, buildFilter, isSubmitting, i18n, config.successToastDescriptor, closeModal]);
	const dateModeOptions = useMemo(
		() => [
			{value: 'all_time' as const, label: i18n._(DATE_MODE_ALL_TIME_DESCRIPTOR)},
			{value: 'custom' as const, label: i18n._(DATE_MODE_CUSTOM_DESCRIPTOR)},
		],
		[i18n.locale],
	);
	const guildFilterOptions = useMemo(
		() => [
			{value: 'exclude' as const, label: i18n._(GUILD_FILTER_MODE_EXCLUDE_DESCRIPTOR)},
			{value: 'include_only' as const, label: i18n._(GUILD_FILTER_MODE_INCLUDE_DESCRIPTOR)},
		],
		[i18n.locale],
	);
	const renderScopeStep = (): React.ReactNode => (
		<div className={styles.step} data-flx="user.privacy-safety-tab.data-request-modal.scope-step">
			<h2
				className={styles.stepTitle}
				data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-scope-step.step-title"
			>
				{i18n._(SCOPE_STEP_TITLE_DESCRIPTOR)}
			</h2>
			<RadioGroup<ScopeValue>
				options={scopeOptions}
				value={scope}
				onChange={setScope}
				aria-label={i18n._(SCOPE_STEP_TITLE_DESCRIPTOR)}
				data-flx="user.privacy-safety-tab.data-request-modal.scope-step.radio-group"
			/>
		</div>
	);
	const renderKindsStep = (): React.ReactNode => (
		<div className={styles.step} data-flx="user.privacy-safety-tab.data-request-modal.kinds-step">
			<h2
				className={styles.stepTitle}
				data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-kinds-step.step-title"
			>
				{i18n._(KINDS_STEP_TITLE_DESCRIPTOR)}
			</h2>
			<p
				className={styles.stepBody}
				data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-kinds-step.step-body"
			>
				{i18n._(KINDS_STEP_BODY_DESCRIPTOR)}
			</p>
			<SwitchGroup data-flx="user.privacy-safety-tab.data-request-modal.kinds-step.switch-group">
				<SwitchGroupItem
					label={i18n._(KIND_DMS_LABEL_DESCRIPTOR)}
					value={includeDms}
					onChange={setIncludeDms}
					data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-kinds-step.switch-group-item.set-include-dms"
				/>
				<SwitchGroupItem
					label={i18n._(KIND_DMS_CLOSED_LABEL_DESCRIPTOR)}
					value={includeDmsClosed}
					onChange={setIncludeDmsClosed}
					data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-kinds-step.switch-group-item.set-include-dms-closed"
				/>
				<SwitchGroupItem
					label={i18n._(KIND_GROUP_DMS_LABEL_DESCRIPTOR)}
					value={includeGroupDms}
					onChange={setIncludeGroupDms}
					data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-kinds-step.switch-group-item.set-include-group-dms"
				/>
				<SwitchGroupItem
					label={i18n._(KIND_COMMUNITIES_LABEL_DESCRIPTOR)}
					value={includeGuilds}
					onChange={setIncludeGuilds}
					data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-kinds-step.switch-group-item.set-include-guilds"
				/>
			</SwitchGroup>
			{needsInclusion && (
				<p
					className={styles.errorText}
					data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-kinds-step.error-text"
				>
					{needsInclusion}
				</p>
			)}
		</div>
	);
	const renderCommunitiesStep = (): React.ReactNode => {
		const isExclude = guildFilterMode === 'exclude';
		return (
			<div className={styles.step} data-flx="user.privacy-safety-tab.data-request-modal.communities-step">
				<h2
					className={styles.stepTitle}
					data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-communities-step.step-title"
				>
					{i18n._(COMMUNITIES_STEP_TITLE_DESCRIPTOR)}
				</h2>
				<Combobox<GuildFilterMode>
					label={i18n._(GUILD_FILTER_MODE_LABEL_DESCRIPTOR)}
					value={guildFilterMode}
					options={guildFilterOptions}
					onChange={setGuildFilterMode}
					data-flx="user.privacy-safety-tab.data-request-modal.communities-step.filter-mode"
				/>
				{guildList.length === 0 ? (
					<div
						className={styles.communityListEmpty}
						data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-communities-step.community-list-empty"
					>
						{i18n._(COMMUNITIES_EMPTY_DESCRIPTOR)}
					</div>
				) : (
					<div
						className={styles.communityList}
						data-flx="user.privacy-safety-tab.data-request-modal.communities-step.list"
					>
						<SwitchGroup data-flx="user.privacy-safety-tab.data-request-modal.communities-step.switch-group">
							{guildList.map((guild) => {
								const isOn = isExclude ? excludedGuildIds.has(guild.id) : includedGuildIds.has(guild.id);
								const onChange = isExclude
									? (next: boolean) => toggleExcludedGuild(guild.id, next)
									: (next: boolean) => toggleIncludedGuild(guild.id, next);
								return (
									<SwitchGroupItem
										key={guild.id}
										label={
											<span
												className={styles.guildRowLabel}
												data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-communities-step.guild-row-label"
											>
												<GuildIcon
													id={guild.id}
													name={guild.name}
													icon={guild.icon}
													sizePx={28}
													data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-communities-step.guild-icon"
												/>
												<span
													className={styles.guildRowName}
													data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-communities-step.guild-row-name"
												>
													{guild.name}
												</span>
											</span>
										}
										value={isOn}
										onChange={onChange}
										data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-communities-step.switch-group-item.change"
									/>
								);
							})}
						</SwitchGroup>
					</div>
				)}
			</div>
		);
	};
	const renderWhenStep = (): React.ReactNode => (
		<div className={styles.step} data-flx="user.privacy-safety-tab.data-request-modal.when-step">
			<h2
				className={styles.stepTitle}
				data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-when-step.step-title"
			>
				{i18n._(WHEN_STEP_TITLE_DESCRIPTOR)}
			</h2>
			<Combobox<DateMode>
				label={i18n._(DATE_MODE_LABEL_DESCRIPTOR)}
				value={dateMode}
				options={dateModeOptions}
				onChange={setDateMode}
				data-flx="user.privacy-safety-tab.data-request-modal.when-step.date-mode"
			/>
			{dateMode === 'custom' && (
				<div
					className={styles.dateFields}
					data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-when-step.date-fields"
				>
					<DateTimePickerField
						label={i18n._(START_DATE_DESCRIPTOR)}
						value={startDate}
						onChange={setStartDate}
						data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-when-step.date-time-picker-field.set-start-date"
					/>
					<DateTimePickerField
						label={i18n._(END_DATE_DESCRIPTOR)}
						value={endDate}
						onChange={setEndDate}
						data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-when-step.date-time-picker-field.set-end-date"
					/>
					<p
						className={styles.helper}
						data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-when-step.helper"
					>
						{i18n._(DATE_HELPER_DESCRIPTOR)}
					</p>
					{dateRangeError && (
						<p
							className={styles.errorText}
							data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-when-step.error-text"
						>
							{dateRangeError}
						</p>
					)}
				</div>
			)}
		</div>
	);
	const renderConfirmStep = (): React.ReactNode => {
		const scopeDescriptor = config.scopeOptionDescriptors.find((opt) => opt.value === scope);
		const scopeLabel = scopeDescriptor ? i18n._(scopeDescriptor.labelDescriptor) : '';
		const conversationParts: Array<string> = [];
		if (isCustomScope) {
			if (includeDms && includeDmsClosed) {
				conversationParts.push(i18n._(SUMMARY_DMS_WITH_CLOSED_DESCRIPTOR));
			} else if (includeDms) {
				conversationParts.push(i18n._(SUMMARY_DMS_OPEN_ONLY_DESCRIPTOR));
			} else if (includeDmsClosed) {
				conversationParts.push(i18n._(SUMMARY_DMS_CLOSED_ONLY_DESCRIPTOR));
			}
			if (includeGroupDms) conversationParts.push(i18n._(SUMMARY_GROUP_DMS_DESCRIPTOR));
			if (includeGuilds) conversationParts.push(i18n._(SUMMARY_COMMUNITIES_INCLUDED_DESCRIPTOR));
		}
		const conversationsValue =
			conversationParts.length > 0 ? conversationParts.join(', ') : i18n._(SUMMARY_NONE_DESCRIPTOR);
		const communityCount = guildFilterMode === 'exclude' ? excludedGuildIds.size : includedGuildIds.size;
		const communitiesValue =
			guildFilterMode === 'exclude'
				? i18n._(SUMMARY_GUILD_EXCLUDE_DESCRIPTOR, {count: communityCount})
				: i18n._(SUMMARY_GUILD_INCLUDE_DESCRIPTOR, {count: communityCount});
		let timeRangeValue: string;
		if (dateMode === 'all_time') {
			timeRangeValue = i18n._(SUMMARY_ALL_TIME_DESCRIPTOR);
		} else if (startDate && endDate) {
			timeRangeValue = i18n._(SUMMARY_BETWEEN_DESCRIPTOR, {
				start: formatDateForSummary(startDate, i18n.locale),
				end: formatDateForSummary(endDate, i18n.locale),
			});
		} else if (startDate) {
			timeRangeValue = i18n._(SUMMARY_FROM_DESCRIPTOR, {start: formatDateForSummary(startDate, i18n.locale)});
		} else if (endDate) {
			timeRangeValue = i18n._(SUMMARY_UNTIL_DESCRIPTOR, {end: formatDateForSummary(endDate, i18n.locale)});
		} else {
			timeRangeValue = i18n._(SUMMARY_ALL_TIME_DESCRIPTOR);
		}
		return (
			<div className={styles.step} data-flx="user.privacy-safety-tab.data-request-modal.confirm-step">
				<h2
					className={styles.stepTitle}
					data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.step-title"
				>
					{i18n._(CONFIRM_STEP_TITLE_DESCRIPTOR)}
				</h2>
				<p
					className={styles.stepBody}
					data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.step-body"
				>
					{i18n._(isCustomScope ? config.confirmCustomDescriptor : config.confirmSimpleDescriptor)}
				</p>
				<div
					className={styles.summaryList}
					data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-list"
				>
					<div
						className={styles.summaryRow}
						data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-row"
					>
						<span
							className={styles.summaryRowLabel}
							data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-row-label"
						>
							{i18n._(SUMMARY_SCOPE_LABEL_DESCRIPTOR)}
						</span>
						<span
							className={styles.summaryRowValue}
							data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-row-value"
						>
							{scopeLabel}
						</span>
					</div>
					{isCustomScope && (
						<div
							className={styles.summaryRow}
							data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-row--2"
						>
							<span
								className={styles.summaryRowLabel}
								data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-row-label--2"
							>
								{i18n._(SUMMARY_CONVERSATIONS_LABEL_DESCRIPTOR)}
							</span>
							<span
								className={styles.summaryRowValue}
								data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-row-value--2"
							>
								{conversationsValue}
							</span>
						</div>
					)}
					{showCommunities && (
						<div
							className={styles.summaryRow}
							data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-row--3"
						>
							<span
								className={styles.summaryRowLabel}
								data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-row-label--3"
							>
								{i18n._(SUMMARY_COMMUNITIES_LABEL_DESCRIPTOR)}
							</span>
							<span
								className={styles.summaryRowValue}
								data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-row-value--3"
							>
								{communitiesValue}
							</span>
						</div>
					)}
					{showWhen && (
						<div
							className={styles.summaryRow}
							data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-row--4"
						>
							<span
								className={styles.summaryRowLabel}
								data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-row-label--4"
							>
								{i18n._(SUMMARY_TIME_RANGE_LABEL_DESCRIPTOR)}
							</span>
							<span
								className={styles.summaryRowValue}
								data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.summary-row-value--4"
							>
								{timeRangeValue}
							</span>
						</div>
					)}
				</div>
				{config.isDelete && (
					<div
						className={styles.dangerNotice}
						data-flx="user.privacy-safety-tab.data-request-modal.data-request-modal.render-confirm-step.danger-notice"
					>
						{i18n._(DELETE_DANGER_NOTICE_DESCRIPTOR)}
					</div>
				)}
			</div>
		);
	};
	const renderStep = (): React.ReactNode => {
		switch (step) {
			case 'scope':
				return renderScopeStep();
			case 'kinds':
				return renderKindsStep();
			case 'communities':
				return renderCommunitiesStep();
			case 'when':
				return renderWhenStep();
			case 'confirm':
				return renderConfirmStep();
		}
	};
	const secondaryLabel = currentStepIndex === 0 ? i18n._(CANCEL_DESCRIPTOR) : i18n._(GO_BACK_DESCRIPTOR);
	return (
		<Modal.Root size="small" centered data-flx="user.privacy-safety-tab.data-request-modal.modal-root">
			<Modal.Header
				title={i18n._(config.titleDescriptor)}
				data-flx="user.privacy-safety-tab.data-request-modal.modal-header"
			/>
			<Modal.Content data-flx="user.privacy-safety-tab.data-request-modal.modal-content">
				<Modal.ContentLayout data-flx="user.privacy-safety-tab.data-request-modal.modal-content-layout">
					<SteppedCarousel
						step={step}
						steps={STEP_ORDER}
						data-flx="user.privacy-safety-tab.data-request-modal.stepped-carousel"
					>
						{renderStep()}
					</SteppedCarousel>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="user.privacy-safety-tab.data-request-modal.modal-footer">
				<Button
					variant="secondary"
					onClick={currentStepIndex === 0 ? closeModal : goBack}
					disabled={isSubmitting}
					data-flx="user.privacy-safety-tab.data-request-modal.button.back"
				>
					{secondaryLabel}
				</Button>
				{isLastStep ? (
					<Button
						variant={config.submitVariant}
						onClick={handleSubmit}
						submitting={isSubmitting}
						data-flx="user.privacy-safety-tab.data-request-modal.button.submit"
					>
						{i18n._(config.submitLabelDescriptor)}
					</Button>
				) : (
					<Button
						onClick={goNext}
						disabled={!canContinue}
						data-flx="user.privacy-safety-tab.data-request-modal.button.continue"
					>
						{i18n._(CONTINUE_DESCRIPTOR)}
					</Button>
				)}
			</Modal.Footer>
		</Modal.Root>
	);
});
export const EXPORT_TAB_DESCRIPTION = EXPORT_TAB_DESCRIPTION_DESCRIPTOR;
export const DELETE_TAB_DESCRIPTION = DELETE_TAB_DESCRIPTION_DESCRIPTOR;
