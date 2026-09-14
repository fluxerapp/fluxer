// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/expressions/components/modals/meme_form/MemeFormFields.module.css';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {Button} from '@app/features/ui/button/Button';
import {Input, Textarea} from '@app/features/ui/components/form/FormInput';
import Users from '@app/features/user/state/Users';
import {MAX_FAVORITE_MEME_TAGS} from '@fluxer/constants/src/LimitConstants';
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
const NAME_MUST_BE_100_CHARACTERS_OR_LESS_DESCRIPTOR = msg({
	message: 'Name must be 100 characters or fewer',
	comment: 'Form validation error for a name that exceeds 100 characters.',
});
const NAME_DESCRIPTOR = msg({
	message: 'Name',
	comment: 'Form field label for the name of the item being edited.',
});
const MY_AWESOME_MEDIA_DESCRIPTOR = msg({
	message: 'My awesome media',
	comment: 'Form placeholder example for a media name input.',
});
const ALT_TEXT_MUST_BE_500_CHARACTERS_OR_LESS_DESCRIPTOR = msg({
	message: 'Alt text must be 500 characters or fewer',
	comment: 'Form validation error for an alt text that is too long.',
});
const ALT_TEXT_DESCRIPTOR = msg({
	message: 'Alt text',
	comment: 'Form field label for the accessibility description of an image or video.',
});
const DESCRIBE_THE_MEDIA_DESCRIPTOR = msg({
	message: 'Describe the media',
	comment: 'Form helper text for the media alt text input.',
});
const ADD_A_TAG_DESCRIPTOR = msg({
	message: 'Add a tag',
	comment: 'Form placeholder for the tag input on an expression form.',
});
const TAGS_COUNT_DESCRIPTOR = msg({
	message: 'Tags ({tagCount}/{tagLimit})',
	comment: 'Tag count label in the favorite meme editor. tagCount is the current number of tags.',
});
const ADD_TAG_BUTTON_DESCRIPTOR = msg({
	message: 'Add',
	comment: 'Button label in the favorite meme editor that adds the typed tag to the tag list.',
});

interface MemeFormFieldsProps {
	form: UseFormReturn<{
		name: string;
		altText?: string;
		tags: Array<string>;
	}>;
	disabled?: boolean;
}

export const MemeFormFields = observer(function MemeFormFields({form, disabled = false}: MemeFormFieldsProps) {
	const {i18n} = useLingui();
	const [tagInput, setTagInput] = useState('');
	const [tags, setTags] = useState<Array<string>>(form.getValues('tags'));
	const currentUser = Users.getCurrentUser();
	const tagLimit = currentUser?.maxFavoriteMemeTags ?? MAX_FAVORITE_MEME_TAGS;
	useEffect(() => {
		form.setValue('tags', tags, {shouldDirty: true});
	}, [tags, form]);
	const handleAddTag = useCallback(() => {
		const trimmedTag = tagInput.trim();
		if (
			trimmedTag &&
			trimmedTag.length >= 1 &&
			trimmedTag.length <= 30 &&
			tags.length < tagLimit &&
			!tags.includes(trimmedTag)
		) {
			setTags([...tags, trimmedTag]);
			setTagInput('');
		}
	}, [tagInput, tags, tagLimit]);
	const handleRemoveTag = useCallback(
		(tagToRemove: string) => {
			setTags(tags.filter((tag) => tag !== tagToRemove));
		},
		[tags],
	);
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
				data-flx="expressions.meme-form.meme-form-fields.input.text"
				{...form.register('name', {
					required: i18n._(NAME_IS_REQUIRED_DESCRIPTOR),
					maxLength: {
						value: 100,
						message: i18n._(NAME_MUST_BE_100_CHARACTERS_OR_LESS_DESCRIPTOR),
					},
				})}
				autoFocus={true}
				type="text"
				label={i18n._(NAME_DESCRIPTOR)}
				placeholder={i18n._(MY_AWESOME_MEDIA_DESCRIPTOR)}
				maxLength={100}
				error={form.formState.errors.name?.message}
				required={true}
				disabled={disabled}
			/>
			<Textarea
				data-flx="expressions.meme-form.meme-form-fields.textarea"
				{...form.register('altText', {
					maxLength: {
						value: 500,
						message: i18n._(ALT_TEXT_MUST_BE_500_CHARACTERS_OR_LESS_DESCRIPTOR),
					},
				})}
				label={i18n._(ALT_TEXT_DESCRIPTOR)}
				placeholder={i18n._(DESCRIBE_THE_MEDIA_DESCRIPTOR)}
				maxLength={500}
				minRows={3}
				maxRows={6}
				error={form.formState.errors.altText?.message}
				disabled={disabled}
			/>
			<div className={styles.tagsContainer} data-flx="expressions.meme-form.meme-form-fields.tags-container">
				<div className={styles.tagsHeader} data-flx="expressions.meme-form.meme-form-fields.tags-header">
					<span className={styles.tagsHeaderLabel} data-flx="expressions.meme-form.meme-form-fields.tags-header-label">
						{i18n._(TAGS_COUNT_DESCRIPTOR, {tagCount: tags.length, tagLimit})}
					</span>
				</div>
				<div className={styles.tagsInputRow} data-flx="expressions.meme-form.meme-form-fields.tags-input-row">
					<Input
						type="text"
						value={tagInput}
						onChange={(e) => setTagInput(e.target.value)}
						onKeyDown={handleKeyDownTag}
						placeholder={i18n._(ADD_A_TAG_DESCRIPTOR)}
						maxLength={30}
						disabled={tags.length >= tagLimit || disabled}
						data-flx="expressions.meme-form.meme-form-fields.input.text--2"
					/>
					<Button
						onClick={handleAddTag}
						disabled={!tagInput.trim() || tags.length >= tagLimit || disabled}
						fitContent
						data-flx="expressions.meme-form.meme-form-fields.button.add-tag"
					>
						{i18n._(ADD_TAG_BUTTON_DESCRIPTOR)}
					</Button>
				</div>
				{tags.length > 0 && (
					<div className={styles.tagsList} data-flx="expressions.meme-form.meme-form-fields.tags-list">
						{tags.map((tag) => (
							<div key={tag} className={styles.tagChip} data-flx="expressions.meme-form.meme-form-fields.tag-chip">
								<span data-flx="expressions.meme-form.meme-form-fields.span">{tag}</span>
								{!disabled && (
									<button
										type="button"
										onClick={() => handleRemoveTag(tag)}
										className={styles.tagRemoveButton}
										data-flx="expressions.meme-form.meme-form-fields.tag-remove-button.remove-tag"
									>
										×
									</button>
								)}
							</div>
						))}
					</div>
				)}
			</div>
		</>
	);
});
