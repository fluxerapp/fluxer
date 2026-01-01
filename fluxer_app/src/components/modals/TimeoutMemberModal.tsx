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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as GuildMemberActionCreators from '~/actions/GuildMemberActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Input, Textarea} from '~/components/form/Input';
import {Select as FormSelect} from '~/components/form/Select';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import type {UserRecord} from '~/records/UserRecord';
import {getTimeoutDurationOptions} from './TimeoutMemberOptions';

interface SelectOption<V extends string | number = number> {
	value: V;
	label: string;
}

interface TimeoutMemberModalProps {
	guildId: string;
	targetUser: UserRecord;
}

const MAX_TIMEOUT_SECONDS = 365 * 24 * 60 * 60;

type CustomDurationUnit = 'seconds' | 'minutes' | 'hours' | 'days';

const CUSTOM_DURATION_MULTIPLIERS: Record<CustomDurationUnit, number> = {
	seconds: 1,
	minutes: 60,
	hours: 60 * 60,
	days: 60 * 60 * 24,
};

export const TimeoutMemberModal: React.FC<TimeoutMemberModalProps> = observer(({guildId, targetUser}) => {
	const {t} = useLingui();

	const getCustomDurationUnitOptions = React.useCallback(
		(): ReadonlyArray<SelectOption<CustomDurationUnit>> => [
			{value: 'seconds', label: t`Seconds`},
			{value: 'minutes', label: t`Minutes`},
			{value: 'hours', label: t`Hours`},
			{value: 'days', label: t`Days`},
		],
		[t],
	);

	const timeoutDurationOptions = React.useMemo(() => getTimeoutDurationOptions(t), [t]);

	const durationOptions = React.useMemo<Array<SelectOption<number | 'custom'>>>(() => {
		const baseOptions = timeoutDurationOptions.map((option) => ({
			value: option.value,
			label: option.label,
		}));
		return [...baseOptions, {value: 'custom' as const, label: t`Custom duration`}];
	}, [timeoutDurationOptions, t]);

	const customDurationUnitOptions = getCustomDurationUnitOptions();

	const [selectedDuration, setSelectedDuration] = React.useState<number | 'custom'>(timeoutDurationOptions[3].value);
	const [customDurationValue, setCustomDurationValue] = React.useState('10');
	const [customDurationUnit, setCustomDurationUnit] = React.useState<CustomDurationUnit>('minutes');
	const [reason, setReason] = React.useState('');
	const [isSubmitting, setIsSubmitting] = React.useState(false);

	const customDurationNumber = Number(customDurationValue);
	const customDurationSeconds =
		Number.isFinite(customDurationNumber) && customDurationNumber > 0
			? customDurationNumber * CUSTOM_DURATION_MULTIPLIERS[customDurationUnit]
			: 0;

	const customDurationError =
		selectedDuration === 'custom'
			? customDurationNumber <= 0
				? t`Duration must be greater than zero.`
				: customDurationSeconds > MAX_TIMEOUT_SECONDS
					? t`Timeout cannot exceed 365 days.`
					: undefined
			: undefined;

	const isCustomDurationValid = selectedDuration !== 'custom' || !customDurationError;
	const effectiveDurationSeconds = selectedDuration === 'custom' ? customDurationSeconds : (selectedDuration as number);

	const handleTimeout = async () => {
		if (selectedDuration === 'custom' && (!isCustomDurationValid || customDurationSeconds <= 0)) {
			return;
		}

		setIsSubmitting(true);
		try {
			const timeoutUntil = new Date(Date.now() + effectiveDurationSeconds * 1000).toISOString();
			const trimmedReason = reason.trim();
			await GuildMemberActionCreators.timeout(guildId, targetUser.id, timeoutUntil, trimmedReason || null);

			ToastActionCreators.createToast({
				type: 'success',
				children: <Trans>Successfully timed out {targetUser.tag}</Trans>,
			});
			ModalActionCreators.pop();
		} catch (error) {
			console.error('Failed to timeout member:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: <Trans>Failed to timeout member. Please try again.</Trans>,
			});
		} finally {
			setIsSubmitting(false);
		}
	};

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Timeout ${targetUser.tag}`} />
			<Modal.Content>
				<div style={{display: 'flex', flexDirection: 'column', gap: '16px'}}>
					<p style={{margin: 0}}>
						<Trans>
							Prevent <strong>{targetUser.tag}</strong> from sending messages, reacting, and joining voice channels for
							the specified duration.
						</Trans>
					</p>

					<FormSelect<number | 'custom'>
						label={t`Timeout Duration`}
						description={t`How long this user should be timed out for.`}
						value={selectedDuration}
						onChange={(value) => setSelectedDuration(value)}
						options={durationOptions}
						disabled={isSubmitting}
					/>

					{selectedDuration === 'custom' && (
						<>
							<div style={{display: 'flex', gap: '12px', alignItems: 'flex-start'}}>
								<Input
									type="number"
									label={t`Custom duration`}
									min={1}
									step={1}
									value={customDurationValue}
									onChange={(event) => setCustomDurationValue(event.target.value)}
									error={customDurationError}
									disabled={isSubmitting}
								/>
								<FormSelect<CustomDurationUnit>
									label={t`Unit`}
									value={customDurationUnit}
									onChange={(value) => setCustomDurationUnit(value)}
									options={customDurationUnitOptions}
									disabled={isSubmitting}
								/>
							</div>

							<p style={{margin: 0, fontSize: '0.85rem', color: 'var(--text-tertiary)'}}>
								<Trans>Enter a numeric value and choose a unit.</Trans>
							</p>

							<p style={{margin: 0, fontSize: '0.85rem', color: 'var(--text-tertiary)'}}>
								<Trans>The timeout cannot exceed 365 days (31536000 seconds).</Trans>
							</p>
						</>
					)}

					<Textarea
						label={t`Reason (optional)`}
						value={reason}
						onChange={(event) => setReason(event.target.value)}
						maxLength={512}
						minRows={3}
						disabled={isSubmitting}
					/>

					<p style={{margin: 0, fontSize: '0.85rem', color: 'var(--text-tertiary)'}}>
						<Trans>This reason will be displayed in the Activity Log in Community Settings.</Trans>
					</p>
				</div>
			</Modal.Content>

			<Modal.Footer>
				<Button variant="secondary" onClick={() => ModalActionCreators.pop()} disabled={isSubmitting}>
					<Trans>Cancel</Trans>
				</Button>
				<Button
					variant="danger-primary"
					onClick={handleTimeout}
					disabled={isSubmitting || (selectedDuration === 'custom' && !isCustomDurationValid)}
				>
					<Trans>Timeout</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
