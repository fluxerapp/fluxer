// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/expressions/components/modals/TagListField.module.css';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {type Dispatch, type SetStateAction, useCallback, useState} from 'react';

const ADD_A_TAG_DESCRIPTOR = msg({
	message: 'Add a tag',
	comment: 'Form placeholder for the tag input on an expression form.',
});
const TAGS_COUNT_DESCRIPTOR = msg({
	message: 'Tags ({tagCount}/{tagLimit})',
	comment: 'Tag count label in the sticker or favorite media editor. tagCount is the current number of tags.',
});
const ADD_TAG_BUTTON_DESCRIPTOR = msg({
	message: 'Add',
	comment: 'Button label in the sticker or favorite media editor that adds the typed tag to the tag list.',
});
interface TagListFieldProps {
	tags: Array<string>;
	onChange: Dispatch<SetStateAction<Array<string>>>;
	limit: number;
	disabled?: boolean;
}
export function TagListField({tags, onChange, limit, disabled = false}: TagListFieldProps) {
	const {i18n} = useLingui();
	const [tagInput, setTagInput] = useState('');
	const handleAddTag = useCallback(() => {
		const trimmedTag = tagInput.trim();
		if (
			trimmedTag &&
			trimmedTag.length >= 1 &&
			trimmedTag.length <= 30 &&
			tags.length < limit &&
			!tags.includes(trimmedTag)
		) {
			onChange((existing) => [...existing, trimmedTag]);
			setTagInput('');
		}
	}, [tagInput, tags, limit, onChange]);
	const handleRemoveTag = useCallback(
		(tagToRemove: string) => {
			onChange((existing) => existing.filter((tag) => tag !== tagToRemove));
		},
		[onChange],
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
		<div className={styles.tagsContainer} data-flx="expressions.sticker-form.sticker-form-fields.tags-container">
			<div className={styles.tagsHeader} data-flx="expressions.sticker-form.sticker-form-fields.tags-header">
				<span className={styles.tagsLabel} data-flx="expressions.sticker-form.sticker-form-fields.tags-label">
					{i18n._(TAGS_COUNT_DESCRIPTOR, {tagCount: tags.length, tagLimit: limit})}
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
					disabled={tags.length >= limit || disabled}
					data-flx="expressions.sticker-form.sticker-form-fields.input.text--3"
				/>
				<Button
					onClick={handleAddTag}
					disabled={!tagInput.trim() || tags.length >= limit || disabled}
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
	);
}
