// SPDX-License-Identifier: AGPL-3.0-or-later

import {LogoutModal} from '@app/features/app/components/dialogs/components/LogoutModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import KeybindManager from '@app/features/app/keybindings/KeybindManager';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/components/date_of_birth/DateOfBirthGate.module.css';
import {DateOfBirthField} from '@app/features/auth/flow/DateOfBirthField';
import {isPlausibleDateOfBirthInput} from '@app/features/auth/flow/DateOfBirthRules';
import {QuietButton} from '@app/features/auth/flow/QuietButton';
import {getAuthErrorMessage} from '@app/features/auth/hooks/useAuthForm';
import {markAgeBlocked} from '@app/features/auth/state/AccountEntryStorage';
import {setDateOfBirthStepOpen} from '@app/features/auth/state/DateOfBirthStep';
import {openGuildWhenAvailable} from '@app/features/guild/commands/GuildVisitCommands';
import {CONTINUE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import ModalState from '@app/features/ui/state/Modal';
import {
	ModalStackContext,
	type ModalStackContextValue,
	UNSTACKED_MODAL_CONTEXT,
} from '@app/features/ui/utils/ModalStackContext';
import * as AccountTierCommands from '@app/features/user/commands/AccountTierCommands';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {type FormEvent, useEffect, useLayoutEffect, useRef, useState} from 'react';

const BEHIND_STACKED_MODAL_PLACEMENT: ModalStackContextValue = {...UNSTACKED_MODAL_CONTEXT, isTopmost: false};

const DATE_OF_BIRTH_TITLE_DESCRIPTOR = msg({
	message: 'When is your birthday?',
	comment: 'Heading of the date of birth step that everyone completes once before using the app.',
});
const DATE_OF_BIRTH_TEXT_DESCRIPTOR = msg({
	message:
		"{productName} uses your date of birth to match content and settings to your age. It isn't shown to anyone. You can't change it later.",
	comment:
		'Text under the date of birth heading. productName is the app name. The last sentence says the date of birth cannot be changed once entered.',
});
const SIGN_OUT_DESCRIPTOR = msg({
	message: 'Sign out',
	comment: 'Quiet text button on the date of birth step that signs out instead.',
});

function ignoreDismiss(): void {}

function formatDateOfBirth(year: string, month: string, day: string): string | null {
	if (!isPlausibleDateOfBirthInput(year, month, day)) return null;
	return `${year.padStart(4, '0')}-${month.padStart(2, '0')}-${day.padStart(2, '0')}`;
}

export const DateOfBirthGate = observer(() => {
	const {i18n} = useLingui();
	const titleRef = useRef<HTMLHeadingElement | null>(null);
	useEffect(() => {
		setDateOfBirthStepOpen(true);
		return () => setDateOfBirthStepOpen(false);
	}, []);
	useLayoutEffect(() => {
		KeybindManager.suspend();
		return () => KeybindManager.resume();
	}, []);
	const [month, setMonth] = useState('');
	const [day, setDay] = useState('');
	const [year, setYear] = useState('');
	const [submitting, setSubmitting] = useState(false);
	const [error, setError] = useState<string | null>(null);
	const dateOfBirth = formatDateOfBirth(year, month, day);
	const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
		event.preventDefault();
		if (dateOfBirth === null || submitting) return;
		setSubmitting(true);
		setError(null);
		try {
			const entry = await AccountTierCommands.setDateOfBirth(dateOfBirth);
			if (entry !== null) {
				void openGuildWhenAvailable(entry.guild_id);
			}
		} catch (caught) {
			if (failureCode(caught) === APIErrorCodes.ACCOUNT_BELOW_MINIMUM_AGE) {
				markAgeBlocked({accountRemoved: true});
				void AuthenticationCommands.logout({notifyServer: false});
				return;
			}
			setError(getAuthErrorMessage(caught, i18n));
		} finally {
			setSubmitting(false);
		}
	};
	const title = i18n._(DATE_OF_BIRTH_TITLE_DESCRIPTOR);
	const stackPlacement = ModalState.hasModalOpen() ? BEHIND_STACKED_MODAL_PLACEMENT : UNSTACKED_MODAL_CONTEXT;
	return (
		<ModalStackContext.Provider value={stackPlacement}>
			<Modal.Root
				size="small"
				centered
				backdropTone="strong"
				disableHistoryManagement
				onClose={ignoreDismiss}
				initialFocusRef={titleRef}
				data-flx="auth.date-of-birth-gate"
			>
				<Modal.ScreenReaderLabel text={title} data-flx="auth.date-of-birth-gate.screen-reader-label" />
				<Modal.Content padding="none" data-flx="auth.date-of-birth-gate.content">
					<form className={styles.form} onSubmit={handleSubmit} data-flx="auth.date-of-birth-gate.form">
						<h1
							ref={titleRef}
							className={styles.title}
							dir="auto"
							tabIndex={-1}
							data-flx="auth.date-of-birth-gate.title"
						>
							{title}
						</h1>
						<p className={styles.description} dir="auto" data-flx="auth.date-of-birth-gate.description">
							{i18n._(DATE_OF_BIRTH_TEXT_DESCRIPTOR, {productName: RuntimeConfig.productName})}
						</p>
						<DateOfBirthField
							selectedMonth={month}
							selectedDay={day}
							selectedYear={year}
							onMonthChange={setMonth}
							onDayChange={setDay}
							onYearChange={setYear}
							error={error ?? undefined}
							data-flx="auth.date-of-birth-gate.date-of-birth-field"
						/>
						<Button
							type="submit"
							fitContainer
							submitting={submitting}
							disabled={dateOfBirth === null}
							data-flx="auth.date-of-birth-gate.button.submit"
						>
							{i18n._(CONTINUE_DESCRIPTOR)}
						</Button>
						<div className={styles.footer} data-flx="auth.date-of-birth-gate.footer">
							<QuietButton
								onClick={() =>
									ModalCommands.push(modal(() => <LogoutModal data-flx="auth.date-of-birth-gate.logout-modal" />))
								}
								data-flx="auth.date-of-birth-gate.button.sign-out"
							>
								{i18n._(SIGN_OUT_DESCRIPTOR)}
							</QuietButton>
						</div>
					</form>
				</Modal.Content>
			</Modal.Root>
		</ModalStackContext.Provider>
	);
});
