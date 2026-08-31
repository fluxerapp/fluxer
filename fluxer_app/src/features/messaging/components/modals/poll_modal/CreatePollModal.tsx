// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import selectorStyles from '@app/features/app/components/dialogs/shared/SelectorModalStyles.module.css';
import {Limits} from '@app/features/app/utils/UserLimits';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import styles from '@app/features/messaging/components/modals/poll_modal/CreatePollModal.module.css';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {FieldSet, Textarea} from '@app/features/ui/components/form/FormInput';
import {Scroller} from '@app/features/ui/components/Scroller';
import {SwitchGroup, SwitchGroupItem} from '@app/features/ui/components/SwitchGroup';
import type {ModalProps} from '@app/features/ui/utils/ModalUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useRef, useState} from 'react';
import {useTextareaSegments} from '../../../hooks/useTextareaSegments';
import {PollAnswerInput} from './PollAnswerInput';

export const CREATE_A_POLL_DESCRIPTOR = msg({
	message: 'Create a poll',
	comment: 'Title of the poll creation modal.',
});
export const POLL_QUESTION_DESCRIPTOR = msg({
	message: 'Question',
	comment: 'Label for the question input.',
});
export const POLL_QUESTION_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'The question you want to ask',
	comment: 'Placeholder text for the question input.',
});
export const POLL_ANSWERS_DESCRIPTOR = msg({
	message: 'Answers',
	comment: 'Label for the answers section of the poll creation modal.',
});
export const POLL_ADD_ANSWER_DESCRIPTOR = msg({
	message: 'Add answer',
	comment: 'Label for the button that adds a new answer to the poll.',
});
export const POLL_DURATION_DESCRIPTOR = msg({
	message: 'Duration',
	comment: 'Label for the duration input.',
});
export const POLL_ALLOW_MULTIPLE_ANSWERS_DESCRIPTOR = msg({
	message: 'Allow multiple answers',
	comment: 'Label for the checkbox that allows users of the poll to select multiple answers.',
});
export const POLL_ANONYMOUS_VOTING_DESCRIPTOR = msg({
	message: 'Anonymous voting',
	comment:
		'Label for the checkbox that makes the poll anonymous (only moderators/admins of the community can see who voted).',
});
export const POLL_SUBMIT_DESCRIPTOR = msg({
	message: 'Submit',
	comment: 'Label for the poll submit button.',
});
export const POLL_ERROR_YOU_FORGOT_TO_ENTER_A_QUESTION = msg({
	message: 'You forgot to enter a question.',
	comment: 'Error message for when no question has been entered in the poll.',
});
export const POLL_ERROR_YOU_SHOULD_ENTER_AT_LEAST_ONE_ANSWER = msg({
	message: 'You should enter at least one answer.',
	comment: 'Error message for when no answer has been entered in the poll.',
});
export const N_WEEKS_DESCRIPTOR = msg({
	message: '{count, plural, one {# week} other {# weeks}}',
	comment: 'Generic description for a duration option that is measured in weeks.',
});
export const N_DAYS_DESCRIPTOR = msg({
	message: '{count, plural, one {# day} other {# days}}',
	comment: 'Generic description for a duration option that is measured in days.',
});
export const N_HOURS_DESCRIPTOR = msg({
	message: '{count, plural, one {# hour} other {# hours}}',
	comment: 'Generic description for a duration option that is measured in hours.',
});

export interface IdlessPollAnswerItem {
	emoji?: FlatEmoji;
	text: string;
}

export interface PollAnswerItem {
	id: number;
	emoji?: FlatEmoji;
	text: string;
}

export interface PollForm {
	question: string;
	answers: Array<PollAnswerItem>;
	duration: number;
	anonymousVoting: boolean;
	allowMultipleAnswers: boolean;
}

interface CreatePollModalProps {
	size?: ModalProps['size'];
	hideCloseButton?: boolean;
	onSubmit: (pollForm: PollForm) => Promise<void> | void;
	disableAutoDismiss?: boolean;
	channelId: string;
}

function durationToLabel(i18n: I18n, hours: number): string {
	if (hours >= 168) {
		const count = Math.floor(hours / 168);
		return i18n._(N_WEEKS_DESCRIPTOR, {count});
	}
	if (hours > 24) {
		const count = Math.floor(hours / 24);
		return i18n._(N_DAYS_DESCRIPTOR, {count});
	}
	return i18n._(N_HOURS_DESCRIPTOR, {count: hours});
}

export function generateDurationOptions(i18n: I18n): ReadonlyArray<ComboboxOption<number>> {
	return [1, 2, 4, 8, 12, 24, 48, 72, 120, 168, 336].map((hours) => ({
		value: hours,
		label: durationToLabel(i18n, hours),
	}));
}

export const CreatePollModal = observer(
	({size = 'small', hideCloseButton, onSubmit, disableAutoDismiss, channelId}: CreatePollModalProps) => {
		const {i18n} = useLingui();
		const initialFocusRef = useRef<HTMLTextAreaElement | null>(null);
		const {previousValueRef, displayToActual, handleTextChange} = useTextareaSegments();
		const [submitting, setSubmitting] = useState(false);

		const [question, setQuestion] = useState('');
		const [forgotToEnterQuestion, setForgotToEnterQuestion] = useState(false);
		const actualQuestion = useMemo(() => displayToActual(question), [question, displayToActual]);
		const maxAnswerCount = Limits.getMaxPollAnswers();
		const maxAnswerLength = Limits.getMaxPollAnswerLength();
		const maxQuestionActualLength = Limits.getMaxPollQuestionLength();
		const questionDisplayMaxLength = Math.max(0, question.length + (maxQuestionActualLength - actualQuestion.length));

		const [duration, setDuration] = useState<number>(24);
		const [forgotToEnterAnswer, setForgotToEnterAnswer] = useState(false);
		const [allowMultipleAnswers, setAllowMultipleAnswers] = useState(false);
		const [anonymousVoting, setAnonymousVoting] = useState(false);
		const [answers, setAnswers] = useState<Array<IdlessPollAnswerItem>>([
			{
				text: '',
			},
			{
				text: '',
			},
		]);

		const durationOptions = useMemo(() => generateDurationOptions(i18n), [i18n]);

		const handleSubmit = useCallback(async () => {
			const selfKey = ModalCommands.getTopModalKey();
			setSubmitting(true);
			try {
				let hasError = false;
				if (question.length === 0) {
					setForgotToEnterQuestion(true);
					hasError = true;
				}
				if (answers.find((answer) => answer.text.length > 0) === undefined) {
					setForgotToEnterAnswer(true);
					hasError = true;
				}
				if (hasError) return;

				await onSubmit({
					question,
					answers: answers
						.filter((answer) => answer.text.length > 0)
						.map((answer, index) => ({id: index + 1, ...answer})),
					duration,
					anonymousVoting,
					allowMultipleAnswers,
				});
				if (!disableAutoDismiss) {
					if (selfKey != null) ModalCommands.popWithKey(selfKey);
					else ModalCommands.pop();
				}
			} finally {
				setSubmitting(false);
			}
		}, [onSubmit, disableAutoDismiss, question, answers, duration, allowMultipleAnswers]);

		return (
			<Modal.Root
				size={size}
				initialFocusRef={initialFocusRef}
				centered
				data-flx="messaging.create-poll-modal.modal-root"
			>
				<Modal.Header
					title={i18n._(CREATE_A_POLL_DESCRIPTOR)}
					hideCloseButton={hideCloseButton}
					data-flx="messaging.create-poll-modal.modal-header"
				/>
				<Modal.Content className={selectorStyles.selectorContent} data-flx="messaging.create-poll-modal.modal-content">
					<div className={selectorStyles.listContainer} data-flx="messaging.create-poll-modal.div">
						<Scroller
							className={selectorStyles.scroller}
							key="create-poll-modal-channel-list-scroller"
							fade={false}
							data-flx="messaging.create-poll-modal.scroller"
						>
							<div className={styles.pollForm} data-flx="messaging.create-poll-modal.div--2">
								<Textarea
									name="question"
									ref={initialFocusRef}
									label={i18n._(POLL_QUESTION_DESCRIPTOR)}
									value={question}
									placeholder={i18n._(POLL_QUESTION_PLACEHOLDER_DESCRIPTOR)}
									maxLength={questionDisplayMaxLength}
									minRows={1}
									maxRows={4}
									showCharacterCount={true}
									error={forgotToEnterQuestion ? i18n._(POLL_ERROR_YOU_FORGOT_TO_ENTER_A_QUESTION) : undefined}
									onChange={(e) => {
										const nativeEvent = e.nativeEvent as InputEvent;
										const newValue = e.target.value;
										const inputType = typeof nativeEvent.inputType === 'string' ? nativeEvent.inputType : undefined;
										setForgotToEnterQuestion(false);
										handleTextChange(newValue, previousValueRef.current, inputType);
										setQuestion(newValue);
									}}
									data-flx="messaging.create-poll-modal.textarea.question"
								/>
								<FieldSet
									label={i18n._(POLL_ANSWERS_DESCRIPTOR)}
									className={styles.answers}
									data-flx="messaging.create-poll-modal.field-set.answers"
								>
									{answers.map((answer, index) => (
										<PollAnswerInput
											key={index}
											textValue={answer.text}
											maxLength={maxAnswerLength}
											onTextChange={(text) => {
												if (index === 0) setForgotToEnterAnswer(false);
												setAnswers((prevAnswers) =>
													prevAnswers.map((prevAnswer, prevIndex) =>
														prevIndex === index ? {...prevAnswer, text: text} : prevAnswer,
													),
												);
											}}
											emojiValue={answer.emoji}
											onEmojiSelect={(emoji) =>
												setAnswers((prevAnswers) =>
													prevAnswers.map((prevAnswer, prevIndex) =>
														prevIndex === index ? {...prevAnswer, emoji} : prevAnswer,
													),
												)
											}
											onDelete={() => {
												if (answers.length <= 1) return;
												setAnswers((prevAnswers) =>
													prevAnswers.filter((_prevAnswer, prevIndex) => prevIndex !== index),
												);
											}}
											error={
												index === 0 && forgotToEnterAnswer
													? i18n._(POLL_ERROR_YOU_SHOULD_ENTER_AT_LEAST_ONE_ANSWER)
													: undefined
											}
											showDelete={answers.length > 1}
											channelId={channelId}
										/>
									))}

									<Button
										leftIcon={<PlusIcon weight="bold" />}
										variant="secondary"
										hidden={answers.length >= maxAnswerCount}
										onClick={() => {
											setAnswers((prevAnswers) => {
												const newId = prevAnswers.length + 1;
												return [
													...prevAnswers,
													{
														id: newId,
														text: '',
													},
												];
											});
										}}
									>
										{i18n._(POLL_ADD_ANSWER_DESCRIPTOR)}
									</Button>
								</FieldSet>
								<Combobox<number>
									label={i18n._(POLL_DURATION_DESCRIPTOR)}
									value={duration}
									options={durationOptions}
									onChange={setDuration}
									isSearchable={false}
									density="compact"
									aria-label={i18n._(POLL_DURATION_DESCRIPTOR)}
									data-flx="messaging.create-poll-modal.combobox.duration"
								/>
								<SwitchGroup data-flx="messaging.create-poll-modal.switch-group">
									<SwitchGroupItem
										value={allowMultipleAnswers}
										label={i18n._(POLL_ALLOW_MULTIPLE_ANSWERS_DESCRIPTOR)}
										onChange={setAllowMultipleAnswers}
										data-flx="messaging.create-poll-modal.switch.allow-multiple-answers"
									/>
									<SwitchGroupItem
										value={anonymousVoting}
										label={i18n._(POLL_ANONYMOUS_VOTING_DESCRIPTOR)}
										onChange={setAnonymousVoting}
										data-flx="messaging.create-poll-modal.switch.anonymous-voting"
									/>
								</SwitchGroup>
							</div>
						</Scroller>
					</div>
				</Modal.Content>
				<Modal.Footer className={styles.modalFooter} data-flx="messaging.create-poll-modal.modal-footer">
					<Button
						onClick={handleSubmit}
						submitting={submitting}
						variant="primary"
						data-flx="messaging.create-poll-modal.button.primary-click"
					>
						{i18n._(POLL_SUBMIT_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
