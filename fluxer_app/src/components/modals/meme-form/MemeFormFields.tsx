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
import {MAX_FAVORITE_MEME_TAGS} from '~/Constants';
import {Input, Textarea} from '~/components/form/Input';
import {Button} from '~/components/uikit/Button/Button';
import styles from './MemeFormFields.module.css';

interface MemeFormFieldsProps {
	form: UseFormReturn<{
		name: string;
		altText?: string;
		tags: Array<string>;
	}>;
	disabled?: boolean;
}

export const MemeFormFields = observer(function MemeFormFields({form, disabled = false}: MemeFormFieldsProps) {
	const {t} = useLingui();
	const [tagInput, setTagInput] = React.useState('');
	const [tags, setTags] = React.useState<Array<string>>(form.getValues('tags'));

	React.useEffect(() => {
		form.setValue('tags', tags, {shouldDirty: true});
	}, [tags, form]);

	const handleAddTag = React.useCallback(() => {
		const trimmedTag = tagInput.trim();
		if (
			trimmedTag &&
			trimmedTag.length >= 1 &&
			trimmedTag.length <= 30 &&
			tags.length < MAX_FAVORITE_MEME_TAGS &&
			!tags.includes(trimmedTag)
		) {
			setTags([...tags, trimmedTag]);
			setTagInput('');
		}
	}, [tagInput, tags]);

	const handleRemoveTag = React.useCallback(
		(tagToRemove: string) => {
			setTags(tags.filter((tag) => tag !== tagToRemove));
		},
		[tags],
	);

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
					maxLength: {
						value: 100,
						message: t`Name must be 100 characters or less`,
					},
				})}
				type="text"
				label={t`Name`}
				placeholder={t`My awesome media`}
				maxLength={100}
				error={form.formState.errors.name?.message}
				autoFocus={true}
				required={true}
				disabled={disabled}
			/>

			<Textarea
				{...form.register('altText', {
					maxLength: {
						value: 500,
						message: t`Alt text must be 500 characters or less`,
					},
				})}
				label={t`Alt Text`}
				placeholder={t`Describe the media`}
				maxLength={500}
				minRows={3}
				maxRows={6}
				error={form.formState.errors.altText?.message}
				disabled={disabled}
			/>

			<div className={styles.tagsContainer}>
				<div className={styles.tagsHeader}>
					<span className={styles.tagsHeaderLabel}>
						<Trans>
							Tags ({tags.length}/{MAX_FAVORITE_MEME_TAGS})
						</Trans>
					</span>
				</div>
				<div className={styles.tagsInputRow}>
					<Input
						type="text"
						value={tagInput}
						onChange={(e) => setTagInput(e.target.value)}
						onKeyDown={handleKeyDownTag}
						placeholder={t`Add a tag`}
						maxLength={30}
						disabled={tags.length >= MAX_FAVORITE_MEME_TAGS || disabled}
					/>
					<Button
						onClick={handleAddTag}
						disabled={!tagInput.trim() || tags.length >= MAX_FAVORITE_MEME_TAGS || disabled}
						fitContent
					>
						<Trans>Add</Trans>
					</Button>
				</div>
				{tags.length > 0 && (
					<div className={styles.tagsList}>
						{tags.map((tag) => (
							<div key={tag} className={styles.tagChip}>
								<span>{tag}</span>
								{!disabled && (
									<button type="button" onClick={() => handleRemoveTag(tag)} className={styles.tagRemoveButton}>
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
