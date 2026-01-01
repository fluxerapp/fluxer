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
import type {UseFormReturn} from 'react-hook-form';
import {Input} from '~/components/form/Input';
import {Button} from '~/components/uikit/Button/Button';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import styles from './StickerFormFields.module.css';

interface StickerFormFieldsProps {
	form: UseFormReturn<{
		name: string;
		description: string;
		tags: Array<string>;
	}>;
	disabled?: boolean;
}

export const StickerFormFields = observer(function StickerFormFields({form, disabled = false}: StickerFormFieldsProps) {
	const {t} = useLingui();
	const [tagInput, setTagInput] = React.useState('');
	const [tags, setTags] = React.useState<Array<string>>(() => {
		const initialTags = form.getValues('tags');
		return Array.isArray(initialTags) ? [...initialTags] : [];
	});

	React.useEffect(() => {
		form.setValue('tags', tags, {shouldDirty: true});
	}, [form, tags]);

	const handleAddTag = React.useCallback(() => {
		const trimmedTag = tagInput.trim();
		if (
			trimmedTag &&
			trimmedTag.length >= 1 &&
			trimmedTag.length <= 30 &&
			tags.length < 10 &&
			!tags.includes(trimmedTag)
		) {
			setTags((existing) => [...existing, trimmedTag]);
			setTagInput('');
		}
	}, [tagInput, tags]);

	const handleRemoveTag = React.useCallback((tagToRemove: string) => {
		setTags((existing) => existing.filter((tag) => tag !== tagToRemove));
	}, []);

	const handleKeyDownTag = React.useCallback(
		(e: React.KeyboardEvent<HTMLInputElement>) => {
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
				{...form.register('name', {
					required: t`Name is required`,
					minLength: {
						value: 2,
						message: t`Name must be at least 2 characters`,
					},
					maxLength: {
						value: 30,
						message: t`Name must be 30 characters or less`,
					},
				})}
				type="text"
				label={t`Name`}
				placeholder={t`My awesome sticker`}
				maxLength={30}
				error={form.formState.errors.name?.message}
				autoFocus={true}
				required={true}
				disabled={disabled}
			/>

			<Input
				{...form.register('description', {
					maxLength: {
						value: 100,
						message: t`Description must be 100 characters or less`,
					},
				})}
				type="text"
				label={t`Description`}
				placeholder={t`Describe the sticker`}
				maxLength={100}
				error={form.formState.errors.description?.message}
				disabled={disabled}
			/>

			<div className={styles.tagsContainer}>
				<div className={styles.tagsHeader}>
					<span className={styles.tagsLabel}>
						<Trans>Tags ({tags.length}/10)</Trans>
					</span>
				</div>
				<div className={styles.tagInputRow}>
					<Input
						type="text"
						value={tagInput}
						onChange={(e) => setTagInput(e.target.value)}
						onKeyDown={handleKeyDownTag}
						placeholder={t`Add a tag`}
						maxLength={30}
						disabled={tags.length >= 10 || disabled}
					/>
					<Button onClick={handleAddTag} disabled={!tagInput.trim() || tags.length >= 10 || disabled} fitContent>
						<Trans>Add</Trans>
					</Button>
				</div>
				{tags.length > 0 && (
					<div className={styles.tagsList}>
						{tags.map((tag) => (
							<div key={tag} className={styles.tag}>
								<span>{tag}</span>
								{!disabled && (
									<FocusRing offset={-2}>
										<button type="button" onClick={() => handleRemoveTag(tag)} className={styles.tagRemoveButton}>
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
