// SPDX-License-Identifier: AGPL-3.0-or-later

import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {showGuildErrorModal} from '@app/features/guild/components/alerts/GuildErrorModalUtils';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildDiscoveryTab.module.css';
import {TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Input, Textarea} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import {getSortedDiscoveryLanguages} from '@app/features/user/utils/LocaleUtils';
import {useRemoteFormReset} from '@app/lib/forms/RemoteFormReset';
import {
	DISCOVERY_DEFAULT_LANGUAGE,
	DISCOVERY_DESCRIPTION_MAX_LENGTH,
	DISCOVERY_DESCRIPTION_MIN_LENGTH,
	DISCOVERY_MAX_TAGS,
	DISCOVERY_TAG_MAX_LENGTH,
	DiscoveryApplicationStatus,
	isValidDiscoveryTag,
	normalizeDiscoveryTag,
} from '@fluxer/constants/src/DiscoveryConstants';
import type {
	DiscoveryApplicationResponse,
	DiscoveryStatusResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {InfoIcon, WarningIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {Controller, useForm} from 'react-hook-form';

const PENDING_DESCRIPTOR = msg({
	message: 'Pending',
	comment:
		'Discovery application status badge in the community Discovery settings tab. The application is awaiting staff review.',
});
const APPROVED_DESCRIPTOR = msg({
	message: 'Approved',
	comment:
		'Discovery application status badge in the community Discovery settings tab. The community has been approved and is listed.',
});
const REJECTED_DESCRIPTOR = msg({
	message: 'Rejected',
	comment:
		'Discovery application status badge in the community Discovery settings tab. The application was declined by staff.',
});
const REMOVED_DESCRIPTOR = msg({
	message: 'Removed',
	comment:
		'Discovery application status badge in the community Discovery settings tab. The community was delisted from Discovery.',
});
const DISCOVERY_TAG_REQUIREMENTS_DESCRIPTOR = msg({
	message: 'Tags must be 2 to {maxLength} characters and alphanumeric.',
	comment: 'Discovery custom-tag validation error. maxLength is the maximum allowed tag length.',
});
const DISCOVERY_TAG_LIMIT_DESCRIPTOR = msg({
	message: 'You can only add up to {maxTags} tags.',
	comment: 'Discovery custom-tag validation error. maxTags is the maximum number of custom tags.',
});
const COULDN_T_WITHDRAW_DISCOVERY_APPLICATION_DESCRIPTOR = msg({
	message: "Couldn't withdraw application",
	comment: 'Error modal title shown when withdrawing a community Discovery application fails.',
});
const COULDN_T_ADD_DISCOVERY_TAG_DESCRIPTOR = msg({
	message: "Couldn't add tag",
	comment: 'Error modal title shown when a custom Discovery tag cannot be added.',
});
const GAMING_DESCRIPTOR = msg({
	message: 'Gaming',
	comment: 'Discovery category dropdown option in the community Discovery settings tab.',
});
const MUSIC_DESCRIPTOR = msg({
	message: 'Music',
	comment: 'Discovery category dropdown option in the community Discovery settings tab.',
});
const ENTERTAINMENT_DESCRIPTOR = msg({
	message: 'Entertainment',
	comment: 'Discovery category dropdown option in the community Discovery settings tab.',
});
const EDUCATION_DESCRIPTOR = msg({
	message: 'Education',
	comment: 'Discovery category dropdown option in the community Discovery settings tab.',
});
const SCIENCE_TECHNOLOGY_DESCRIPTOR = msg({
	message: 'Science & technology',
	comment: 'Discovery category dropdown option in the community Discovery settings tab.',
});
const CONTENT_CREATOR_DESCRIPTOR = msg({
	message: 'Content creator',
	comment:
		'Discovery category dropdown option in the community Discovery settings tab. Communities run by an individual creator.',
});
const ANIME_MANGA_DESCRIPTOR = msg({
	message: 'Anime & manga',
	comment: 'Discovery category dropdown option in the community Discovery settings tab.',
});
const MOVIES_TV_DESCRIPTOR = msg({
	message: 'Movies & TV',
	comment:
		'Discovery category dropdown option in the community Discovery settings tab. "TV" is the abbreviation for television.',
});
const OTHER_DESCRIPTOR = msg({
	message: 'Other',
	comment: 'Catch-all Discovery category dropdown option in the community Discovery settings tab.',
});
const A_DESCRIPTION_IS_REQUIRED_DESCRIPTOR = msg({
	message: 'A description is required.',
	comment: 'Inline validation error on the Discovery application form when the description field is empty.',
});
const DESCRIPTION_MUST_BE_AT_LEAST_CHARACTERS_DESCRIPTOR = msg({
	message: 'Description must be at least {discoveryDescriptionMinLength} characters.',
	comment:
		'Inline validation error on the Discovery application form when the description is too short. {discoveryDescriptionMinLength} is an integer.',
});
const DESCRIPTION_MUST_BE_NO_MORE_THAN_CHARACTERS_DESCRIPTOR = msg({
	message: 'Description must be no more than {discoveryDescriptionMaxLength} characters.',
	comment:
		'Inline validation error on the Discovery application form when the description is too long. {discoveryDescriptionMaxLength} is an integer.',
});
const DESCRIBE_WHAT_YOUR_COMMUNITY_IS_ABOUT_DESCRIPTOR = msg({
	message: 'Describe what your community is about',
	comment: 'Placeholder in the description textarea on the Discovery application form.',
});
const REMOVE_TAG_DESCRIPTOR = msg({
	message: 'Remove tag {tag}',
	comment:
		'Accessible label for the per-tag X button in the Discovery tag list. {tag} is the tag text. Used by screen readers.',
});
const ADD_A_TAG_AND_PRESS_ENTER_DESCRIPTOR = msg({
	message: 'Add a tag and press Enter',
	comment: 'Placeholder in the tag input on the Discovery application form. "Enter" refers to the Enter/Return key.',
});
const logger = new Logger('GuildDiscoveryTab');

interface FormInputs {
	description: string;
	category_type: number;
	primary_language: string;
	custom_tags: Array<string>;
}

function getEmptyDiscoveryFormValues(): FormInputs {
	return {
		description: '',
		category_type: 0,
		primary_language: DISCOVERY_DEFAULT_LANGUAGE,
		custom_tags: [],
	};
}

function getDiscoveryApplicationFormValues(application: DiscoveryApplicationResponse): FormInputs {
	return {
		description: application.description,
		category_type: application.category_type,
		primary_language: application.primary_language ?? DISCOVERY_DEFAULT_LANGUAGE,
		custom_tags: application.custom_tags ?? [],
	};
}

function StatusBadge({status}: {status: string}) {
	const {i18n} = useLingui();
	const statusConfig: Record<string, {label: string; className: string}> = useMemo(
		() => ({
			[DiscoveryApplicationStatus.PENDING]: {label: i18n._(PENDING_DESCRIPTOR), className: styles.statusPending},
			[DiscoveryApplicationStatus.APPROVED]: {label: i18n._(APPROVED_DESCRIPTOR), className: styles.statusApproved},
			[DiscoveryApplicationStatus.REJECTED]: {label: i18n._(REJECTED_DESCRIPTOR), className: styles.statusRejected},
			[DiscoveryApplicationStatus.REMOVED]: {label: i18n._(REMOVED_DESCRIPTOR), className: styles.statusRemoved},
		}),
		[i18n.locale],
	);
	const config = statusConfig[status];
	if (!config) return null;
	return (
		<span
			className={clsx(styles.statusBadge, config.className)}
			data-flx="guild.guild-tabs.guild-discovery-tab.status-badge.status-badge"
		>
			{config.label}
		</span>
	);
}

const GuildDiscoveryTab: React.FC<{guildId: string}> = ({guildId}) => {
	const {i18n} = useLingui();
	const [status, setStatus] = useState<DiscoveryStatusResponse | null>(null);
	const [isLoading, setIsLoading] = useState(true);
	const [isWithdrawing, setIsWithdrawing] = useState(false);
	const categoryOptions: ReadonlyArray<ComboboxOption<number>> = useMemo(
		() => [
			{value: 0, label: i18n._(GAMING_DESCRIPTOR)},
			{value: 1, label: i18n._(MUSIC_DESCRIPTOR)},
			{value: 2, label: i18n._(ENTERTAINMENT_DESCRIPTOR)},
			{value: 3, label: i18n._(EDUCATION_DESCRIPTOR)},
			{value: 4, label: i18n._(SCIENCE_TECHNOLOGY_DESCRIPTOR)},
			{value: 5, label: i18n._(CONTENT_CREATOR_DESCRIPTOR)},
			{value: 6, label: i18n._(ANIME_MANGA_DESCRIPTOR)},
			{value: 7, label: i18n._(MOVIES_TV_DESCRIPTOR)},
			{value: 8, label: i18n._(OTHER_DESCRIPTOR)},
		],
		[i18n.locale],
	);
	const languageOptions: ReadonlyArray<ComboboxOption<string>> = useMemo(
		() =>
			getSortedDiscoveryLanguages().map((language) => ({
				value: language.code,
				label: language.label,
			})),
		[i18n],
	);
	const [tagInput, setTagInput] = useState('');
	const fetchStatus = useCallback(async () => {
		try {
			setIsLoading(true);
			const data = await GuildCommands.getDiscoveryStatus(guildId);
			setStatus(data);
		} catch (err) {
			logger.error('Failed to fetch discovery status', err);
		} finally {
			setIsLoading(false);
		}
	}, [guildId]);
	useEffect(() => {
		void fetchStatus();
	}, [fetchStatus]);
	const application = status?.application ?? null;
	const eligible = status?.eligible ?? false;
	const minMemberCount = status?.min_member_count ?? 0;
	const hasActiveApplication =
		application != null &&
		(application.status === DiscoveryApplicationStatus.PENDING ||
			application.status === DiscoveryApplicationStatus.APPROVED);
	const canApply =
		!hasActiveApplication &&
		(application == null ||
			application.status === DiscoveryApplicationStatus.REJECTED ||
			application.status === DiscoveryApplicationStatus.REMOVED);
	const remoteValues = useMemo<FormInputs>(
		() =>
			hasActiveApplication && application
				? getDiscoveryApplicationFormValues(application)
				: getEmptyDiscoveryFormValues(),
		[hasActiveApplication, application],
	);
	const form = useForm<FormInputs>({
		defaultValues: getEmptyDiscoveryFormValues(),
	});
	const {commitRemoteValues} = useRemoteFormReset<FormInputs>({
		form,
		identityKey: `${guildId}:${hasActiveApplication ? (application?.applied_at ?? 'active') : 'new'}`,
		remoteValues,
	});
	const setApplicationFromResponse = useCallback((response: DiscoveryApplicationResponse) => {
		setStatus((prev) => (prev ? {...prev, application: response} : prev));
	}, []);
	const onSubmit = useCallback(
		async (data: FormInputs) => {
			const payload = {
				description: data.description,
				category_type: data.category_type,
				primary_language: data.primary_language,
				custom_tags: data.custom_tags,
			};
			if (hasActiveApplication) {
				const result = await GuildCommands.updateDiscoveryApplication(guildId, payload);
				setApplicationFromResponse(result);
				commitRemoteValues(getDiscoveryApplicationFormValues(result));
				ToastCommands.createToast({
					type: 'success',
					children: <Trans>Discovery listing updated</Trans>,
				});
			} else {
				const result = await GuildCommands.applyForDiscovery(guildId, payload);
				setApplicationFromResponse(result);
				commitRemoteValues(getDiscoveryApplicationFormValues(result));
				ToastCommands.createToast({
					type: 'success',
					children: <Trans>Discovery application sent</Trans>,
				});
			}
		},
		[guildId, hasActiveApplication, form, setApplicationFromResponse],
	);
	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'description',
	});
	const handleWithdraw = useCallback(async () => {
		try {
			setIsWithdrawing(true);
			await GuildCommands.withdrawDiscoveryApplication(guildId);
			setStatus((prev) => (prev ? {...prev, application: null} : prev));
			commitRemoteValues(getEmptyDiscoveryFormValues());
			ToastCommands.createToast({
				type: 'success',
				children: <Trans>Discovery application withdrawn</Trans>,
			});
		} catch (err) {
			logger.error('Failed to withdraw discovery application', err);
			showGuildErrorModal({
				title: i18n._(COULDN_T_WITHDRAW_DISCOVERY_APPLICATION_DESCRIPTOR),
				message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx: 'guild.guild-tabs.guild-discovery-tab.withdraw-application-error-modal',
			});
		} finally {
			setIsWithdrawing(false);
		}
	}, [guildId, commitRemoteValues, i18n]);
	if (isLoading) {
		return (
			<div className={styles.spinnerContainer} data-flx="guild.guild-tabs.guild-discovery-tab.spinner-container">
				<Spinner data-flx="guild.guild-tabs.guild-discovery-tab.spinner" />
			</div>
		);
	}
	return (
		<div className={styles.container} data-flx="guild.guild-tabs.guild-discovery-tab.container">
			<div className={styles.header} data-flx="guild.guild-tabs.guild-discovery-tab.header">
				<h2 className={styles.title} data-flx="guild.guild-tabs.guild-discovery-tab.title">
					<Trans>Discovery</Trans>
				</h2>
				<p className={styles.subtitle} data-flx="guild.guild-tabs.guild-discovery-tab.subtitle">
					<Trans>List your community in Discovery so others can find and join it.</Trans>
				</p>
			</div>
			{!eligible && canApply && (
				<div className={styles.warning} data-flx="guild.guild-tabs.guild-discovery-tab.warning">
					<div className={styles.warningContent} data-flx="guild.guild-tabs.guild-discovery-tab.warning-content">
						<div className={styles.warningIcon} data-flx="guild.guild-tabs.guild-discovery-tab.warning-icon">
							<WarningIcon
								size={remFromPx(20)}
								weight="fill"
								data-flx="guild.guild-tabs.guild-discovery-tab.warning-icon--2"
							/>
						</div>
						<div className={styles.warningBody} data-flx="guild.guild-tabs.guild-discovery-tab.warning-body">
							<p className={styles.warningTitle} data-flx="guild.guild-tabs.guild-discovery-tab.warning-title">
								<Trans>Not enough members</Trans>
							</p>
							<p className={styles.warningText} data-flx="guild.guild-tabs.guild-discovery-tab.warning-text">
								<Trans>
									Your community needs at least {minMemberCount} members before it can be listed in Discovery.
								</Trans>
							</p>
						</div>
					</div>
				</div>
			)}
			{application != null && (
				<div className={styles.statusCard} data-flx="guild.guild-tabs.guild-discovery-tab.status-card">
					<div className={styles.statusRow} data-flx="guild.guild-tabs.guild-discovery-tab.status-row">
						<span className={styles.statusLabel} data-flx="guild.guild-tabs.guild-discovery-tab.status-label">
							<Trans>Status:</Trans>
						</span>
						<StatusBadge status={application.status} data-flx="guild.guild-tabs.guild-discovery-tab.status-badge" />
					</div>
					{(application.removal_reason ?? application.review_reason) && (
						<p className={styles.reviewReason} data-flx="guild.guild-tabs.guild-discovery-tab.review-reason">
							<Trans>Reason: {application.removal_reason ?? application.review_reason}</Trans>
						</p>
					)}
				</div>
			)}
			{application?.status === DiscoveryApplicationStatus.APPROVED && (
				<div className={styles.info} data-flx="guild.guild-tabs.guild-discovery-tab.info">
					<div className={styles.infoContent} data-flx="guild.guild-tabs.guild-discovery-tab.info-content">
						<div className={styles.infoIcon} data-flx="guild.guild-tabs.guild-discovery-tab.info-icon">
							<InfoIcon
								size={remFromPx(20)}
								weight="fill"
								data-flx="guild.guild-tabs.guild-discovery-tab.info-icon--2"
							/>
						</div>
						<p className={styles.infoText} data-flx="guild.guild-tabs.guild-discovery-tab.info-text">
							<Trans>
								Your community is listed in Discovery. You can update your listing details below or withdraw to remove
								it.
							</Trans>
						</p>
					</div>
				</div>
			)}
			{application?.status === DiscoveryApplicationStatus.PENDING && (
				<div className={styles.info} data-flx="guild.guild-tabs.guild-discovery-tab.info--2">
					<div className={styles.infoContent} data-flx="guild.guild-tabs.guild-discovery-tab.info-content--2">
						<div className={styles.infoIcon} data-flx="guild.guild-tabs.guild-discovery-tab.info-icon--3">
							<InfoIcon
								size={remFromPx(20)}
								weight="fill"
								data-flx="guild.guild-tabs.guild-discovery-tab.info-icon--4"
							/>
						</div>
						<p className={styles.infoText} data-flx="guild.guild-tabs.guild-discovery-tab.info-text--2">
							<Trans>
								Your application is pending review. You can still update your listing details or withdraw the
								application.
							</Trans>
						</p>
					</div>
				</div>
			)}
			{(canApply || hasActiveApplication) && (
				<Form form={form} onSubmit={handleSubmit} data-flx="guild.guild-tabs.guild-discovery-tab.form.submit">
					<div className={styles.formCard} data-flx="guild.guild-tabs.guild-discovery-tab.form-card">
						<div data-flx="guild.guild-tabs.guild-discovery-tab.div">
							<div className={styles.fieldLabel} data-flx="guild.guild-tabs.guild-discovery-tab.field-label">
								<Trans>Description</Trans>
							</div>
							<Controller
								name="description"
								control={form.control}
								rules={{
									required: i18n._(A_DESCRIPTION_IS_REQUIRED_DESCRIPTOR),
									minLength: {
										value: DISCOVERY_DESCRIPTION_MIN_LENGTH,
										message: i18n._(DESCRIPTION_MUST_BE_AT_LEAST_CHARACTERS_DESCRIPTOR, {
											discoveryDescriptionMinLength: DISCOVERY_DESCRIPTION_MIN_LENGTH,
										}),
									},
									maxLength: {
										value: DISCOVERY_DESCRIPTION_MAX_LENGTH,
										message: i18n._(DESCRIPTION_MUST_BE_NO_MORE_THAN_CHARACTERS_DESCRIPTOR, {
											discoveryDescriptionMaxLength: DISCOVERY_DESCRIPTION_MAX_LENGTH,
										}),
									},
								}}
								render={({field, fieldState}) => (
									<Textarea
										name={field.name}
										value={field.value}
										onChange={field.onChange}
										onBlur={field.onBlur}
										ref={field.ref}
										error={fieldState.error?.message}
										label=""
										placeholder={i18n._(DESCRIBE_WHAT_YOUR_COMMUNITY_IS_ABOUT_DESCRIPTOR)}
										minRows={3}
										maxRows={6}
										maxLength={DISCOVERY_DESCRIPTION_MAX_LENGTH}
										showCharacterCount
										disabled={!eligible && canApply}
										data-flx="guild.guild-tabs.guild-discovery-tab.textarea.change"
									/>
								)}
								data-flx="guild.guild-tabs.guild-discovery-tab.controller"
							/>
						</div>
						<div data-flx="guild.guild-tabs.guild-discovery-tab.div--2">
							<div className={styles.fieldLabel} data-flx="guild.guild-tabs.guild-discovery-tab.field-label--2">
								<Trans>Category</Trans>
							</div>
							<Controller
								name="category_type"
								control={form.control}
								render={({field}) => (
									<Combobox<number>
										value={field.value}
										onChange={field.onChange}
										options={categoryOptions}
										isSearchable={false}
										disabled={!eligible && canApply}
										data-flx="guild.guild-tabs.guild-discovery-tab.select.change"
									/>
								)}
								data-flx="guild.guild-tabs.guild-discovery-tab.controller--2"
							/>
							<p className={styles.helpText} data-flx="guild.guild-tabs.guild-discovery-tab.help-text">
								<Trans>Choose the category that best describes your community. You can change this any time.</Trans>
							</p>
						</div>
						<div data-flx="guild.guild-tabs.guild-discovery-tab.div--3">
							<div className={styles.fieldLabel} data-flx="guild.guild-tabs.guild-discovery-tab.field-label--3">
								<Trans>Primary language</Trans>
							</div>
							<Controller
								name="primary_language"
								control={form.control}
								render={({field}) => (
									<Combobox<string>
										value={field.value}
										onChange={field.onChange}
										options={languageOptions}
										isSearchable
										disabled={!eligible && canApply}
										data-flx="guild.guild-tabs.guild-discovery-tab.select.change--2"
									/>
								)}
								data-flx="guild.guild-tabs.guild-discovery-tab.controller--3"
							/>
							<p className={styles.helpText} data-flx="guild.guild-tabs.guild-discovery-tab.help-text--2">
								<Trans>The language most of your community speaks. Used to filter Discovery results.</Trans>
							</p>
						</div>
						<div data-flx="guild.guild-tabs.guild-discovery-tab.div--4">
							<div className={styles.fieldLabel} data-flx="guild.guild-tabs.guild-discovery-tab.field-label--4">
								<Trans>Custom tags</Trans>
							</div>
							<Controller
								name="custom_tags"
								control={form.control}
								render={({field}) => {
									const tags: Array<string> = field.value ?? [];
									const addTag = (rawValue: string) => {
										const value = normalizeDiscoveryTag(rawValue);
										if (!value) return;
										if (!isValidDiscoveryTag(value)) {
											showGuildErrorModal({
												title: i18n._(COULDN_T_ADD_DISCOVERY_TAG_DESCRIPTOR),
												message: i18n._(DISCOVERY_TAG_REQUIREMENTS_DESCRIPTOR, {
													maxLength: DISCOVERY_TAG_MAX_LENGTH,
												}),
												dataFlx: 'guild.guild-tabs.guild-discovery-tab.invalid-tag-error-modal',
											});
											return;
										}
										if (tags.includes(value)) return;
										if (tags.length >= DISCOVERY_MAX_TAGS) {
											showGuildErrorModal({
												title: i18n._(COULDN_T_ADD_DISCOVERY_TAG_DESCRIPTOR),
												message: i18n._(DISCOVERY_TAG_LIMIT_DESCRIPTOR, {maxTags: DISCOVERY_MAX_TAGS}),
												dataFlx: 'guild.guild-tabs.guild-discovery-tab.tag-limit-error-modal',
											});
											return;
										}
										field.onChange([...tags, value]);
										setTagInput('');
									};
									const removeTag = (tag: string) => {
										field.onChange(tags.filter((t) => t !== tag));
									};
									return (
										<div className={styles.tagsField} data-flx="guild.guild-tabs.guild-discovery-tab.tags-field">
											<div className={styles.tagsList} data-flx="guild.guild-tabs.guild-discovery-tab.tags-list">
												{tags.map((tag) => (
													<span
														key={tag}
														className={styles.tagChip}
														data-flx="guild.guild-tabs.guild-discovery-tab.tag-chip"
													>
														{tag}
														<button
															type="button"
															aria-label={i18n._(REMOVE_TAG_DESCRIPTOR, {tag})}
															className={styles.tagChipRemove}
															onClick={() => removeTag(tag)}
															disabled={!eligible && canApply}
															data-flx="guild.guild-tabs.guild-discovery-tab.tag-chip-remove.remove-tag.button"
														>
															×
														</button>
													</span>
												))}
											</div>
											<div
												className={styles.tagsInputRow}
												data-flx="guild.guild-tabs.guild-discovery-tab.tags-input-row"
											>
												<Input
													value={tagInput}
													onChange={(e) => setTagInput(e.target.value)}
													onKeyDown={(e) => {
														if (e.key === 'Enter' || e.key === ',') {
															e.preventDefault();
															addTag(tagInput);
														} else if (e.key === 'Backspace' && tagInput.length === 0 && tags.length > 0) {
															removeTag(tags[tags.length - 1]!);
														}
													}}
													maxLength={DISCOVERY_TAG_MAX_LENGTH}
													placeholder={i18n._(ADD_A_TAG_AND_PRESS_ENTER_DESCRIPTOR)}
													disabled={(!eligible && canApply) || tags.length >= DISCOVERY_MAX_TAGS}
													data-flx="guild.guild-tabs.guild-discovery-tab.input"
												/>
												<Button
													type="button"
													small
													variant="secondary"
													onClick={() => addTag(tagInput)}
													disabled={
														(!eligible && canApply) || tags.length >= DISCOVERY_MAX_TAGS || tagInput.trim().length === 0
													}
													data-flx="guild.guild-tabs.guild-discovery-tab.button.add-tag"
												>
													<Trans>Add</Trans>
												</Button>
											</div>
										</div>
									);
								}}
								data-flx="guild.guild-tabs.guild-discovery-tab.controller--4"
							/>
							<p className={styles.helpText} data-flx="guild.guild-tabs.guild-discovery-tab.help-text--3">
								<Trans>
									Up to {DISCOVERY_MAX_TAGS} tags help people find your community. They show up in Discovery search.
								</Trans>
							</p>
						</div>
						<div className={styles.actions} data-flx="guild.guild-tabs.guild-discovery-tab.actions">
							{hasActiveApplication && (
								<Button
									type="button"
									variant="danger"
									onClick={handleWithdraw}
									submitting={isWithdrawing}
									data-flx="guild.guild-tabs.guild-discovery-tab.button.withdraw"
								>
									<Trans>Withdraw</Trans>
								</Button>
							)}
							<Button
								type="submit"
								submitting={isSubmitting}
								disabled={!eligible && canApply}
								data-flx="guild.guild-tabs.guild-discovery-tab.button.submit"
							>
								{hasActiveApplication ? <Trans>Save</Trans> : <Trans>Apply</Trans>}
							</Button>
						</div>
					</div>
				</Form>
			)}
		</div>
	);
};

export default GuildDiscoveryTab;
