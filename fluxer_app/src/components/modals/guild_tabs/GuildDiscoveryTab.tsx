/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import * as GuildActionCreators from '@app/actions/GuildActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {Form} from '@app/components/form/Form';
import {Textarea} from '@app/components/form/Input';
import {Select, type SelectOption} from '@app/components/form/Select';
import styles from '@app/components/modals/guild_tabs/GuildDiscoveryTab.module.css';
import {Button} from '@app/components/uikit/button/Button';
import {Spinner} from '@app/components/uikit/Spinner';
import {useFormSubmit} from '@app/hooks/useFormSubmit';
import {Logger} from '@app/lib/Logger';
import {
	DISCOVERY_DESCRIPTION_MAX_LENGTH,
	DISCOVERY_DESCRIPTION_MIN_LENGTH,
	DiscoveryApplicationStatus,
} from '@fluxer/constants/src/DiscoveryConstants';
import type {
	DiscoveryApplicationResponse,
	DiscoveryStatusResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import {Trans, useLingui} from '@lingui/react/macro';
import {InfoIcon, WarningIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {Controller, useForm} from 'react-hook-form';

const logger = new Logger('GuildDiscoveryTab');

interface FormInputs {
	description: string;
	category_type: number;
}

function StatusBadge({status}: {status: string}) {
	const {t} = useLingui();

	const statusConfig: Record<string, {label: string; className: string}> = useMemo(
		() => ({
			[DiscoveryApplicationStatus.PENDING]: {label: t`Pending`, className: styles.statusPending},
			[DiscoveryApplicationStatus.APPROVED]: {label: t`Approved`, className: styles.statusApproved},
			[DiscoveryApplicationStatus.REJECTED]: {label: t`Rejected`, className: styles.statusRejected},
			[DiscoveryApplicationStatus.REMOVED]: {label: t`Removed`, className: styles.statusRemoved},
		}),
		[t],
	);

	const config = statusConfig[status];
	if (!config) return null;

	return <span className={clsx(styles.statusBadge, config.className)}>{config.label}</span>;
}

const GuildDiscoveryTab: React.FC<{guildId: string}> = ({guildId}) => {
	const {t} = useLingui();
	const [status, setStatus] = useState<DiscoveryStatusResponse | null>(null);
	const [isLoading, setIsLoading] = useState(true);
	const [isWithdrawing, setIsWithdrawing] = useState(false);

	const categoryOptions: ReadonlyArray<SelectOption<number>> = useMemo(
		() => [
			{value: 0, label: t`Gaming`},
			{value: 1, label: t`Music`},
			{value: 2, label: t`Entertainment`},
			{value: 3, label: t`Education`},
			{value: 4, label: t`Science & Technology`},
			{value: 5, label: t`Content Creator`},
			{value: 6, label: t`Anime & Manga`},
			{value: 7, label: t`Movies & TV`},
			{value: 8, label: t`Other`},
		],
		[t],
	);

	const fetchStatus = useCallback(async () => {
		try {
			setIsLoading(true);
			const data = await GuildActionCreators.getDiscoveryStatus(guildId);
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

	const formValues = useMemo(
		() =>
			hasActiveApplication && application
				? {description: application.description, category_type: application.category_type}
				: undefined,
		[hasActiveApplication, application],
	);

	const form = useForm<FormInputs>({
		defaultValues: {
			description: '',
			category_type: 0,
		},
		values: formValues,
	});

	const setApplicationFromResponse = useCallback((response: DiscoveryApplicationResponse) => {
		setStatus((prev) => (prev ? {...prev, application: response} : prev));
	}, []);

	const onSubmit = useCallback(
		async (data: FormInputs) => {
			if (hasActiveApplication) {
				const result = await GuildActionCreators.updateDiscoveryApplication(guildId, {
					description: data.description,
					category_type: data.category_type,
				});
				setApplicationFromResponse(result);
				form.reset(data);
				ToastActionCreators.createToast({
					type: 'success',
					children: <Trans>Discovery listing updated</Trans>,
				});
			} else {
				const result = await GuildActionCreators.applyForDiscovery(guildId, {
					description: data.description,
					category_type: data.category_type,
				});
				setApplicationFromResponse(result);
				form.reset(data);
				ToastActionCreators.createToast({
					type: 'success',
					children: <Trans>Discovery application submitted</Trans>,
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
			await GuildActionCreators.withdrawDiscoveryApplication(guildId);
			setStatus((prev) => (prev ? {...prev, application: null} : prev));
			form.reset({description: '', category_type: 0});
			ToastActionCreators.createToast({
				type: 'success',
				children: <Trans>Discovery application withdrawn</Trans>,
			});
		} catch (err) {
			logger.error('Failed to withdraw discovery application', err);
			ToastActionCreators.createToast({
				type: 'error',
				children: <Trans>Failed to withdraw application. Please try again.</Trans>,
			});
		} finally {
			setIsWithdrawing(false);
		}
	}, [guildId, form]);

	if (isLoading) {
		return (
			<div className={styles.spinnerContainer}>
				<Spinner />
			</div>
		);
	}

	return (
		<div className={styles.container}>
			<div className={styles.header}>
				<h2 className={styles.title}>
					<Trans>Discovery</Trans>
				</h2>
				<p className={styles.subtitle}>
					<Trans>List your community in Discovery so others can find and join it.</Trans>
				</p>
			</div>

			{!eligible && canApply && (
				<div className={styles.warning}>
					<div className={styles.warningContent}>
						<div className={styles.warningIcon}>
							<WarningIcon size={20} weight="fill" />
						</div>
						<div className={styles.warningBody}>
							<p className={styles.warningTitle}>
								<Trans>Not enough members</Trans>
							</p>
							<p className={styles.warningText}>
								<Trans>
									Your community needs at least {minMemberCount} members before it can be listed in Discovery.
								</Trans>
							</p>
						</div>
					</div>
				</div>
			)}

			{application != null && (
				<div className={styles.statusCard}>
					<div className={styles.statusRow}>
						<span className={styles.statusLabel}>
							<Trans>Status:</Trans>
						</span>
						<StatusBadge status={application.status} />
					</div>
					{application.review_reason && (
						<p className={styles.reviewReason}>
							<Trans>Reason: {application.review_reason}</Trans>
						</p>
					)}
				</div>
			)}

			{application?.status === DiscoveryApplicationStatus.APPROVED && (
				<div className={styles.info}>
					<div className={styles.infoContent}>
						<div className={styles.infoIcon}>
							<InfoIcon size={20} weight="fill" />
						</div>
						<p className={styles.infoText}>
							<Trans>
								Your community is listed in Discovery. You can update your listing details below or withdraw to remove
								it.
							</Trans>
						</p>
					</div>
				</div>
			)}

			{application?.status === DiscoveryApplicationStatus.PENDING && (
				<div className={styles.info}>
					<div className={styles.infoContent}>
						<div className={styles.infoIcon}>
							<InfoIcon size={20} weight="fill" />
						</div>
						<p className={styles.infoText}>
							<Trans>
								Your application is pending review. You can still update your listing details or withdraw the
								application.
							</Trans>
						</p>
					</div>
				</div>
			)}

			{(canApply || hasActiveApplication) && (
				<Form form={form} onSubmit={handleSubmit}>
					<div className={styles.formCard}>
						<div>
							<div className={styles.fieldLabel}>
								<Trans>Description</Trans>
							</div>
							<Controller
								name="description"
								control={form.control}
								rules={{
									required: t`A description is required.`,
									minLength: {
										value: DISCOVERY_DESCRIPTION_MIN_LENGTH,
										message: t`Description must be at least ${DISCOVERY_DESCRIPTION_MIN_LENGTH} characters.`,
									},
									maxLength: {
										value: DISCOVERY_DESCRIPTION_MAX_LENGTH,
										message: t`Description must be no more than ${DISCOVERY_DESCRIPTION_MAX_LENGTH} characters.`,
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
										placeholder={t`Describe what your community is about...`}
										minRows={3}
										maxRows={6}
										maxLength={DISCOVERY_DESCRIPTION_MAX_LENGTH}
										showCharacterCount
										disabled={!eligible && canApply}
									/>
								)}
							/>
						</div>

						<div>
							<div className={styles.fieldLabel}>
								<Trans>Category</Trans>
							</div>
							<Controller
								name="category_type"
								control={form.control}
								render={({field}) => (
									<Select<number>
										value={field.value}
										onChange={field.onChange}
										options={categoryOptions}
										isSearchable={false}
										disabled={!eligible && canApply}
									/>
								)}
							/>
							<p className={styles.helpText}>
								<Trans>Choose the category that best describes your community.</Trans>
							</p>
						</div>

						<div className={styles.actions}>
							{hasActiveApplication && (
								<Button type="button" variant="danger-secondary" onClick={handleWithdraw} submitting={isWithdrawing}>
									<Trans>Withdraw</Trans>
								</Button>
							)}
							<Button type="submit" submitting={isSubmitting} disabled={!eligible && canApply}>
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
