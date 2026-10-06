// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/expressions/components/modals/sticker_form/StickerFormFields.module.css';
import {DESCRIPTION_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';
import type {UseFormReturn} from 'react-hook-form';

const NAME_IS_REQUIRED_DESCRIPTOR = msg({
	message: 'Name is required',
	comment: 'Form validation error shown when the name field is empty.',
});
const NAME_MUST_BE_AT_LEAST_2_CHARACTERS_DESCRIPTOR = msg({
	message: 'Name must be at least 2 characters',
	comment: 'Form validation error for a name that is too short.',
});
const NAME_MUST_BE_30_CHARACTERS_OR_LESS_DESCRIPTOR = msg({
	message: 'Name must be 30 characters or fewer',
	comment: 'Form validation error for a name that exceeds 30 characters.',
});
const NAME_DESCRIPTOR = msg({
	message: 'Name',
	comment: 'Form field label for the name of the item being edited.',
});
const MY_AWESOME_STICKER_DESCRIPTOR = msg({
	message: 'My awesome sticker',
	comment: 'Form placeholder example for a sticker name input.',
});
const DESCRIPTION_MUST_BE_500_CHARACTERS_OR_LESS_DESCRIPTOR = msg({
	message: 'Description must be 500 characters or fewer',
	comment: 'Form validation error for a description that is too long.',
});
const DESCRIBE_THE_STICKER_DESCRIPTOR = msg({
	message: 'Describe the sticker',
	comment: 'Form helper text for the sticker description input.',
});
const ADD_A_TAG_DESCRIPTOR = msg({
	message: 'Add a tag',
	comment: 'Form placeholder for the tag input on an expression form.',
});
const STICKER_TAG_LIMIT = 10;
const TAGS_COUNT_DESCRIPTOR = msg({
	message: 'Tags ({tagCount}/{tagLimit})',
	comment: 'Tag count label in the sticker editor. tagCount is the current number of tags.',
});
const ADD_TAG_BUTTON_DESCRIPTOR = msg({
	message: 'Add',
	comment: 'Button label in the sticker editor that adds the typed tag to the tag list.',
});

interface StickerFormFieldsProps {
	form: UseFormReturn<{
		name: string;
		description: string;
		tags: Array<string>;
	}>;
	disabled?: boolean;
}

export const StickerFormFields = observer(function StickerFormFields({form, disabled = false}: StickerFormFieldsProps) {
	const {i18n} = useLingui();
	const [tagInput, setTagInput] = useState('');
	const [tags, setTags] = useState<Array<string>>(() => {
		const initialTags = form.getValues('tags');
		return Array.isArray(initialTags) ? [...initialTags] : [];
	});
	useEffect(() => {
		form.setValue('tags', tags, {shouldDirty: true});
	}, [form, tags]);
	const handleAddTag = useCallback(() => {
		const trimmedTag = tagInput.trim();
		if (
			trimmedTag &&
			trimmedTag.length >= 1 &&
			trimmedTag.length <= 30 &&
			tags.length < STICKER_TAG_LIMIT &&
			!tags.includes(trimmedTag)
		) {
			setTags((existing) => [...existing, trimmedTag]);
			setTagInput('');
		}
	}, [tagInput, tags]);
	const handleRemoveTag = useCallback((tagToRemove: string) => {
		setTags((existing) => existing.filter((tag) => tag !== tagToRemove));
	}, []);
	const handleKeyDownTag = useCallback(
		(e: React.KeyboardEvent<HTMLInputElement>) => {
			if (isIMEComposing(e)) {
				return;
			}
			if (e.key === 'Enter') {
				e.preventDefault();
				handleAddTag();
			}
		},
		[handleAddTag],
	);
	return (
		<>
			<Input
				data-flx="expressions.sticker-form.sticker-form-fields.input.text"
				{...form.register('name', {
					required: i18n._(NAME_IS_REQUIRED_DESCRIPTOR),
					minLength: {
						value: 2,
						message: i18n._(NAME_MUST_BE_AT_LEAST_2_CHARACTERS_DESCRIPTOR),
					},
					maxLength: {
						value: 30,
						message: i18n._(NAME_MUST_BE_30_CHARACTERS_OR_LESS_DESCRIPTOR),
					},
				})}
				autoFocus={true}
				type="text"
				label={i18n._(NAME_DESCRIPTOR)}
				placeholder={i18n._(MY_AWESOME_STICKER_DESCRIPTOR)}
				maxLength={30}
				error={form.formState.errors.name?.message}
				required={true}
				disabled={disabled}
			/>
			<Input
				data-flx="expressions.sticker-form.sticker-form-fields.input.text--2"
				{...form.register('description', {
					maxLength: {
						value: 500,
						message: i18n._(DESCRIPTION_MUST_BE_500_CHARACTERS_OR_LESS_DESCRIPTOR),
					},
				})}
				type="text"
				label={i18n._(DESCRIPTION_DESCRIPTOR)}
				placeholder={i18n._(DESCRIBE_THE_STICKER_DESCRIPTOR)}
				maxLength={500}
				error={form.formState.errors.description?.message}
				disabled={disabled}
			/>
			<div className={styles.tagsContainer} data-flx="expressions.sticker-form.sticker-form-fields.tags-container">
				<div className={styles.tagsHeader} data-flx="expressions.sticker-form.sticker-form-fields.tags-header">
					<span className={styles.tagsLabel} data-flx="expressions.sticker-form.sticker-form-fields.tags-label">
						{i18n._(TAGS_COUNT_DESCRIPTOR, {tagCount: tags.length, tagLimit: STICKER_TAG_LIMIT})}
					</span>
				</div>
				<div className={styles.tagInputRow} data-flx="expressions.sticker-form.sticker-form-fields.tag-input-row">
					<Input
						type="text"
						value={tagInput}
						onChange={(e) => setTagInput(e.target.value)}
						onKeyDown={handleKeyDownTag}
						placeholder={i18n._(ADD_A_TAG_DESCRIPTOR)}
						maxLength={30}
						disabled={tags.length >= STICKER_TAG_LIMIT || disabled}
						data-flx="expressions.sticker-form.sticker-form-fields.input.text--3"
					/>
					<Button
						onClick={handleAddTag}
						disabled={!tagInput.trim() || tags.length >= STICKER_TAG_LIMIT || disabled}
						fitContent
						data-flx="expressions.sticker-form.sticker-form-fields.button.add-tag"
					>
						{i18n._(ADD_TAG_BUTTON_DESCRIPTOR)}
					</Button>
				</div>
				{tags.length > 0 && (
					<div className={styles.tagsList} data-flx="expressions.sticker-form.sticker-form-fields.tags-list">
						{tags.map((tag) => (
							<div key={tag} className={styles.tag} data-flx="expressions.sticker-form.sticker-form-fields.tag">
								<span data-flx="expressions.sticker-form.sticker-form-fields.span">{tag}</span>
								{!disabled && (
									<FocusRing offset={-2} data-flx="expressions.sticker-form.sticker-form-fields.focus-ring">
										<button
											type="button"
											onClick={() => handleRemoveTag(tag)}
											className={styles.tagRemoveButton}
											data-flx="expressions.sticker-form.sticker-form-fields.tag-remove-button.remove-tag"
										>
											×
										</button>
									</FocusRing>
								)}
							</div>
						))}
					</div>
				)}
			</div>
		</>
	);
});
