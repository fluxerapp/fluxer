// SPDX-License-Identifier: AGPL-3.0-or-later

import selectorStyles from '@app/features/app/components/dialogs/shared/SelectorModalStyles.module.css';
import {Limits} from '@app/features/app/utils/UserLimits';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ExpressionPickerSheet} from '@app/features/expressions/components/modals/ExpressionPickerSheet';
import styles from '@app/features/messaging/components/modals/poll_modal/CreatePollModal.module.css';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Button} from '@app/features/ui/button/Button';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {FieldSet, Textarea} from '@app/features/ui/components/form/FormInput';
import {Scroller} from '@app/features/ui/components/Scroller';
import {SwitchGroup, SwitchGroupItem} from '@app/features/ui/components/SwitchGroup';
import {useLingui} from '@lingui/react';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useState} from 'react';
import {useTextareaSegments} from '../../../hooks/useTextareaSegments';
import {
	CREATE_A_POLL_DESCRIPTOR,
	generateDurationOptions,
	POLL_ADD_ANSWER_DESCRIPTOR,
	POLL_ALLOW_MULTIPLE_ANSWERS_DESCRIPTOR,
	POLL_ANONYMOUS_VOTING_DESCRIPTOR,
	POLL_ANSWERS_DESCRIPTOR,
	POLL_DURATION_DESCRIPTOR,
	POLL_ERROR_YOU_FORGOT_TO_ENTER_A_QUESTION,
	POLL_ERROR_YOU_SHOULD_ENTER_AT_LEAST_ONE_ANSWER,
	POLL_QUESTION_DESCRIPTOR,
	POLL_QUESTION_PLACEHOLDER_DESCRIPTOR,
	POLL_SUBMIT_DESCRIPTOR,
	type IdlessPollAnswerItem,
	type PollForm,
} from './CreatePollModal';
import {EmojiContextMenuBottomSheet, PollAnswerInput} from './PollAnswerInput';

interface CreatePollBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	onSubmit: (pollForm: PollForm) => Promise<void> | void;
	channelId: string;
}

export const CreatePollBottomSheet = observer(({isOpen, onClose, onSubmit, channelId}: CreatePollBottomSheetProps) => {
	const {i18n} = useLingui();
	const {previousValueRef, displayToActual, handleTextChange} = useTextareaSegments();
	const [submitting, setSubmitting] = useState(false);
	const [currentOpenedAnswer, setCurrentOpenedAnswer] = useState(-1);
	const [expressionPickerOpen, setExpressionPickerOpen] = useState(false);
	const [emojiContextMenuOpen, setEmojiContextMenuOpen] = useState(false);

	const [question, setQuestion] = useState('');
	const [forgotToEnterQuestion, setForgotToEnterQuestion] = useState(false);
	const actualQuestion = useMemo(() => displayToActual(question), [question, displayToActual]);
	const maxAnswerCount = Limits.getMaxPollAnswers();
	const maxAnswerLength = Limits.getMaxPollAnswerLength();
	const maxQuestionActualLength = Limits.getMaxPollQuestionLength();
	const questionDisplayMaxLength = Math.max(0, question.length + (maxQuestionActualLength - actualQuestion.length));

	const defaultAnswerState = [
		{
			text: '',
		},
		{
			text: '',
		},
	];
	const defaultDuration = 24;
	const durationOptions = useMemo(() => generateDurationOptions(i18n), [i18n]);

	const [duration, setDuration] = useState<number>(defaultDuration);
	const [forgotToEnterAnswer, setForgotToEnterAnswer] = useState(false);
	const [allowMultipleAnswers, setAllowMultipleAnswers] = useState(false);
	const [anonymousVoting, setAnonymousVoting] = useState(false);
	const [answers, setAnswers] = useState<Array<IdlessPollAnswerItem>>(defaultAnswerState);

	const handleSubmit = useCallback(async () => {
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

			setQuestion('');
			setCurrentOpenedAnswer(-1);
			setAnswers(defaultAnswerState);
			setDuration(defaultDuration);
			setAllowMultipleAnswers(false);
		} finally {
			setSubmitting(false);
		}
	}, [onSubmit, question, answers, duration, allowMultipleAnswers]);
	const handleEmojiSelect = useCallback((answerIndex: number, emoji?: FlatEmoji) => {
		setAnswers((prevAnswers) =>
			prevAnswers.map((prevAnswer, prevIndex) => (prevIndex === answerIndex ? {...prevAnswer, emoji} : prevAnswer)),
		);
	}, []);

	return (
		<BottomSheet
			isOpen={isOpen}
			onClose={onClose}
			snapPoints={[0, 0.7, 1]}
			initialSnap={2}
			title={i18n._(CREATE_A_POLL_DESCRIPTOR)}
			showCloseButton={false}
			data-flx="messaging.create-poll-bottom-sheet.bottom-sheet"
		>
			<div className={selectorStyles.listContainer} data-flx="messaging.create-poll-bottom-sheet.div">
				<Scroller
					className={selectorStyles.scroller}
					key="create-poll-modal-channel-list-scroller"
					fade={false}
					data-flx="messaging.create-poll-bottom-sheet.scroller"
				>
					<div className={styles.pollForm} data-flx="messaging.create-poll-bottom-sheet.div--2">
						<Textarea
							name="question"
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
							data-flx="messaging.create-poll-bottom-sheet.textarea.question"
						/>
						<FieldSet
							label={i18n._(POLL_ANSWERS_DESCRIPTOR)}
							className={styles.answers}
							data-flx="messaging.create-poll-bottom-sheet.field-set.answers"
						>
							{answers.map((answer, index) => (
								<PollAnswerInput
									key={index}
									textValue={answer.text}
									maxLength={maxAnswerLength}
									isMobile={true}
									onOpenEmojiSheet={() => {
										setCurrentOpenedAnswer(index);
										setExpressionPickerOpen(true);
									}}
									onOpenEmojiContextMenuSheet={() => {
										setCurrentOpenedAnswer(index);
										setEmojiContextMenuOpen(true);
									}}
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
										setAnswers((prevAnswers) => prevAnswers.filter((_prevAnswer, prevIndex) => prevIndex !== index));
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
							data-flx="messaging.create-poll-bottom-sheet.combobox.duration"
						/>
						<SwitchGroup data-flx="messaging.create-poll-bottom-sheet.switch-group">
							<SwitchGroupItem
								label={i18n._(POLL_ALLOW_MULTIPLE_ANSWERS_DESCRIPTOR)}
								value={allowMultipleAnswers}
								onChange={setAllowMultipleAnswers}
								data-flx="messaging.create-poll-bottom-sheet.switch-group-item.voice-channel-double-click"
							/>
							<SwitchGroupItem
								value={anonymousVoting}
								label={i18n._(POLL_ANONYMOUS_VOTING_DESCRIPTOR)}
								onChange={setAnonymousVoting}
								data-flx="messaging.create-poll-modal.switch.anonymous-voting"
							/>
						</SwitchGroup>
						<Button
							onClick={handleSubmit}
							submitting={submitting}
							variant="primary"
							data-flx="messaging.create-poll-bottom-sheet.button.primary-click"
						>
							{i18n._(POLL_SUBMIT_DESCRIPTOR)}
						</Button>
					</div>
				</Scroller>
			</div>
			<ExpressionPickerSheet
				isOpen={expressionPickerOpen}
				onClose={() => setExpressionPickerOpen(false)}
				channelId={channelId}
				onEmojiSelect={(emoji) => handleEmojiSelect(currentOpenedAnswer, emoji)}
				data-flx="messaging.create-poll-bottom-sheet.expression-picker-sheet"
			/>
			<EmojiContextMenuBottomSheet
				isOpen={emojiContextMenuOpen}
				currentOpenedAnswer={currentOpenedAnswer}
				onClose={() => setEmojiContextMenuOpen(false)}
				openExpressionPicker={(answerIndex) => {
					setCurrentOpenedAnswer(answerIndex);
					setExpressionPickerOpen(true);
				}}
				onEmojiSelect={(answerIndex, emoji) => {
					setCurrentOpenedAnswer(answerIndex);
					handleEmojiSelect(answerIndex, emoji);
				}}
				data-flx="messaging.create-poll-bottom-sheet.emoji-context-menu-sheet"
			/>
		</BottomSheet>
	);
});
