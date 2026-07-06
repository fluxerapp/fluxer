// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import styles from '@app/features/channel/components/modals/PollCreateModal.module.css';
import {CANCEL_DESCRIPTOR, CREATE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {CloudAttachment} from '@app/features/messaging/upload/CloudUpload';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {
	MAX_POLL_DURATION_SECONDS,
	MAX_POLL_OPTION_TEXT_LENGTH,
	MAX_POLL_OPTIONS,
	MAX_POLL_TITLE_LENGTH,
	MIN_POLL_DURATION_SECONDS,
	MIN_POLL_OPTIONS,
} from '@fluxer/constants/src/LimitConstants';
import type {PollRequest} from '@fluxer/schema/src/domains/message/PollSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PlusIcon, XIcon} from '@phosphor-icons/react';
import {type FormEvent, useCallback, useMemo, useRef, useState} from 'react';

const CREATE_POLL_DESCRIPTOR = msg({
	message: 'Create poll',
	comment: 'Dialog title for creating a poll from the chat composer.',
});
const QUESTION_DESCRIPTOR = msg({
	message: 'Question',
	comment: 'Input label for the poll title/question.',
});
const OPTION_DESCRIPTOR = msg({
	message: 'Option {number}',
	comment: 'Input label for a numbered poll option. Preserve {number}.',
});
const ADD_OPTION_DESCRIPTOR = msg({
	message: 'Add option',
	comment: 'Button label for adding another poll option.',
});
const REMOVE_OPTION_DESCRIPTOR = msg({
	message: 'Remove option',
	comment: 'Accessible label for removing a poll option row.',
});
const DURATION_HOURS_DESCRIPTOR = msg({
	message: 'Duration (hours)',
	comment: 'Input label for poll duration in hours.',
});
const ANONYMOUS_RESPONSES_DESCRIPTOR = msg({
	message: 'Anonymous responses',
	comment: 'Switch label for hiding poll voter identity.',
});
const RANKED_CHOICE_DESCRIPTOR = msg({
	message: 'Ranked choice',
	comment: 'Switch label for allowing ordered poll choices.',
});
const CUSTOM_ANSWERS_DESCRIPTOR = msg({
	message: 'Custom answers',
	comment: 'Switch label for allowing users to add text poll options.',
});
const NO_IMAGE_DESCRIPTOR = msg({
	message: 'No image',
	comment: 'Accessible label for clearing a poll option image selection.',
});
const USE_IMAGE_DESCRIPTOR = msg({
	message: 'Use {filename}',
	comment: 'Accessible label for selecting an uploaded image for a poll option. Preserve {filename}.',
});
const POLL_NEEDS_QUESTION_DESCRIPTOR = msg({
	message: 'Add a question.',
	comment: 'Validation error in poll creation modal.',
});
const POLL_NEEDS_OPTIONS_DESCRIPTOR = msg({
	message: 'Add at least two unique options.',
	comment: 'Validation error in poll creation modal.',
});
const POLL_DURATION_INVALID_DESCRIPTOR = msg({
	message: 'Choose a duration from 1 hour to 30 days.',
	comment: 'Validation error in poll creation modal.',
});
const POLL_IMAGE_MISSING_DESCRIPTOR = msg({
	message: 'One selected image is no longer attached.',
	comment: 'Validation error in poll creation modal when an image assigned to an option was removed from the composer.',
});

interface PollCreateModalProps {
	onSubmit: (poll: PollRequest) => void;
	onClose: () => void;
	availableAttachments?: ReadonlyArray<CloudAttachment>;
	'data-flx'?: string;
}

interface PollOptionDraft {
	key: string;
	text: string;
	attachmentId: number | null;
}

function createOptionDraft(key: string): PollOptionDraft {
	return {key, text: '', attachmentId: null};
}

function getAttachmentPreviewUrl(attachment: CloudAttachment): string | null {
	return attachment.thumbnailURL ?? attachment.previewURL;
}

function isImageAttachment(attachment: CloudAttachment): boolean {
	return attachment.file.type.toLowerCase().startsWith('image/') && getAttachmentPreviewUrl(attachment) != null;
}

function getUniqueTrimmedOptions(options: ReadonlyArray<PollOptionDraft>): Array<PollOptionDraft> {
	const seen = new Set<string>();
	const result: Array<PollOptionDraft> = [];
	for (const option of options) {
		const text = option.text.trim();
		const key = text.toLowerCase();
		if (!text || seen.has(key)) {
			continue;
		}
		seen.add(key);
		result.push({...option, text});
	}
	return result;
}

export function PollCreateModal({onSubmit, onClose, availableAttachments = []}: PollCreateModalProps) {
	const {i18n} = useLingui();
	const [title, setTitle] = useState('');
	const [options, setOptions] = useState<Array<PollOptionDraft>>([createOptionDraft('0'), createOptionDraft('1')]);
	const [durationHours, setDurationHours] = useState('24');
	const [anonymous, setAnonymous] = useState(false);
	const [rankedChoice, setRankedChoice] = useState(false);
	const [customAnswers, setCustomAnswers] = useState(false);
	const [error, setError] = useState<string | null>(null);
	const nextOptionKeyRef = useRef(2);
	const maxDurationHours = useMemo(() => Math.floor(MAX_POLL_DURATION_SECONDS / 3600), []);
	const availableImageAttachments = useMemo(
		() => availableAttachments.filter(isImageAttachment),
		[availableAttachments],
	);
	const attachmentRequestIdByUploadId = useMemo(
		() => new Map(availableAttachments.map((attachment, index) => [attachment.id, index])),
		[availableAttachments],
	);
	const updateOption = useCallback((index: number, value: string) => {
		setOptions((current) =>
			current.map((option, optionIndex) => (optionIndex === index ? {...option, text: value} : option)),
		);
	}, []);
	const updateOptionAttachment = useCallback((index: number, attachmentId: number | null) => {
		setOptions((current) =>
			current.map((option, optionIndex) => (optionIndex === index ? {...option, attachmentId} : option)),
		);
	}, []);
	const addOption = useCallback(() => {
		setOptions((current) => {
			if (current.length >= MAX_POLL_OPTIONS) {
				return current;
			}
			const key = String(nextOptionKeyRef.current++);
			return [...current, createOptionDraft(key)];
		});
	}, []);
	const removeOption = useCallback((index: number) => {
		setOptions((current) => {
			if (current.length <= MIN_POLL_OPTIONS) {
				return current;
			}
			return current.filter((_option, optionIndex) => optionIndex !== index);
		});
	}, []);
	const handleSubmit = useCallback(
		(event: FormEvent<HTMLFormElement>) => {
			event.preventDefault();
			const trimmedTitle = title.trim();
			if (!trimmedTitle) {
				setError(i18n._(POLL_NEEDS_QUESTION_DESCRIPTOR));
				return;
			}
			const trimmedOptions = getUniqueTrimmedOptions(options);
			if (trimmedOptions.length < MIN_POLL_OPTIONS) {
				setError(i18n._(POLL_NEEDS_OPTIONS_DESCRIPTOR));
				return;
			}
			const hasMissingImage = trimmedOptions.some(
				(option) => option.attachmentId != null && !attachmentRequestIdByUploadId.has(option.attachmentId),
			);
			if (hasMissingImage) {
				setError(i18n._(POLL_IMAGE_MISSING_DESCRIPTOR));
				return;
			}
			const duration = Number(durationHours);
			const durationSeconds = Math.round(duration * 3600);
			if (
				!Number.isFinite(duration) ||
				durationSeconds < MIN_POLL_DURATION_SECONDS ||
				durationSeconds > MAX_POLL_DURATION_SECONDS
			) {
				setError(i18n._(POLL_DURATION_INVALID_DESCRIPTOR));
				return;
			}
			onSubmit({
				title: trimmedTitle,
				options: trimmedOptions.map((option) => ({
					text: option.text,
					...(option.attachmentId != null
						? {attachment_id: attachmentRequestIdByUploadId.get(option.attachmentId)}
						: {}),
				})),
				duration_seconds: durationSeconds,
				anonymous,
				allow_ranked_choice: rankedChoice,
				allow_custom_answers: customAnswers,
			});
			onClose();
		},
		[
			anonymous,
			attachmentRequestIdByUploadId,
			customAnswers,
			durationHours,
			i18n,
			onClose,
			onSubmit,
			options,
			rankedChoice,
			title,
		],
	);
	return (
		<Modal.Root size="small" centered onClose={onClose} data-flx="channel.poll-create-modal.modal-root">
			<form onSubmit={handleSubmit} data-flx="channel.poll-create-modal.form.submit">
				<Modal.Header title={i18n._(CREATE_POLL_DESCRIPTOR)} data-flx="channel.poll-create-modal.modal-header" />
				<Modal.Content contentClassName={styles.content} data-flx="channel.poll-create-modal.modal-content">
					<Input
						value={title}
						onChange={(event) => setTitle(event.target.value)}
						label={i18n._(QUESTION_DESCRIPTOR)}
						maxLength={MAX_POLL_TITLE_LENGTH}
						autoFocus={true}
						required={true}
						data-flx="channel.poll-create-modal.question-input"
					/>
					<div className={styles.options} data-flx="channel.poll-create-modal.options">
						{options.map((option, index) => (
							<div className={styles.optionRow} key={option.key} data-flx="channel.poll-create-modal.option-row">
								<div className={styles.optionFields} data-flx="channel.poll-create-modal.option-fields">
									<Input
										value={option.text}
										onChange={(event) => updateOption(index, event.target.value)}
										label={i18n._(OPTION_DESCRIPTOR, {number: index + 1})}
										maxLength={MAX_POLL_OPTION_TEXT_LENGTH}
										required={index < MIN_POLL_OPTIONS}
										data-flx="channel.poll-create-modal.option-input"
									/>
									{availableImageAttachments.length > 0 && (
										<div className={styles.imageChoices} data-flx="channel.poll-create-modal.image-choices">
											<button
												type="button"
												className={styles.imageChoiceButton}
												aria-label={i18n._(NO_IMAGE_DESCRIPTOR)}
												aria-pressed={option.attachmentId == null}
												onClick={() => updateOptionAttachment(index, null)}
												data-flx="channel.poll-create-modal.image-choice-button.clear"
											>
												<XIcon size={14} weight="bold" data-flx="channel.poll-create-modal.clear-image-icon" />
											</button>
											{availableImageAttachments.map((attachment) => {
												const previewUrl = getAttachmentPreviewUrl(attachment);
												if (!previewUrl) {
													return null;
												}
												const selected = option.attachmentId === attachment.id;
												return (
													<button
														type="button"
														key={attachment.id}
														className={styles.imageChoiceButton}
														aria-label={i18n._(USE_IMAGE_DESCRIPTOR, {filename: attachment.filename})}
														aria-pressed={selected}
														onClick={() => updateOptionAttachment(index, attachment.id)}
														data-flx="channel.poll-create-modal.image-choice-button.select"
													>
														<img
															src={previewUrl}
															alt=""
															className={styles.imageChoicePreview}
															data-flx="channel.poll-create-modal.image-choice-preview"
														/>
													</button>
												);
											})}
										</div>
									)}
								</div>
								<button
									type="button"
									className={styles.optionRemoveButton}
									aria-label={i18n._(REMOVE_OPTION_DESCRIPTOR)}
									disabled={options.length <= MIN_POLL_OPTIONS}
									onClick={() => removeOption(index)}
									data-flx="channel.poll-create-modal.remove-option-button.remove"
								>
									<XIcon size={14} weight="bold" data-flx="channel.poll-create-modal.x-icon" />
								</button>
							</div>
						))}
						<Button
							type="button"
							variant="secondary"
							className={styles.addOptionButton}
							disabled={options.length >= MAX_POLL_OPTIONS}
							onClick={addOption}
							data-flx="channel.poll-create-modal.add-option-button.add"
						>
							<PlusIcon size={14} weight="bold" data-flx="channel.poll-create-modal.plus-icon" />
							{i18n._(ADD_OPTION_DESCRIPTOR)}
						</Button>
					</div>
					<Input
						value={durationHours}
						onChange={(event) => setDurationHours(event.target.value)}
						label={i18n._(DURATION_HOURS_DESCRIPTOR)}
						min={Math.ceil(MIN_POLL_DURATION_SECONDS / 3600)}
						max={maxDurationHours}
						step={1}
						type="number"
						required={true}
						data-flx="channel.poll-create-modal.duration-input"
					/>
					<div className={styles.switches} data-flx="channel.poll-create-modal.switches">
						<Switch
							value={anonymous}
							onChange={setAnonymous}
							label={i18n._(ANONYMOUS_RESPONSES_DESCRIPTOR)}
							compact
							data-flx="channel.poll-create-modal.switch.anonymous"
						/>
						<Switch
							value={rankedChoice}
							onChange={setRankedChoice}
							label={i18n._(RANKED_CHOICE_DESCRIPTOR)}
							compact
							data-flx="channel.poll-create-modal.switch.ranked-choice"
						/>
						<Switch
							value={customAnswers}
							onChange={setCustomAnswers}
							label={i18n._(CUSTOM_ANSWERS_DESCRIPTOR)}
							compact
							data-flx="channel.poll-create-modal.switch.custom-answers"
						/>
					</div>
					{error && (
						<p className={styles.error} role="alert" data-flx="channel.poll-create-modal.error">
							{error}
						</p>
					)}
				</Modal.Content>
				<Modal.Footer data-flx="channel.poll-create-modal.modal-footer">
					<Button onClick={onClose} variant="secondary" data-flx="channel.poll-create-modal.cancel-button">
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button type="submit" data-flx="channel.poll-create-modal.create-button.submit">
						{i18n._(CREATE_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</form>
		</Modal.Root>
	);
}
