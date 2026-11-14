// SPDX-License-Identifier: AGPL-3.0-or-later

import {TagListField} from '@app/features/expressions/components/modals/TagListField';
import {DESCRIPTION_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';
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

const STICKER_TAG_LIMIT = 10;

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

	const [tags, setTags] = useState<Array<string>>(() => {
		const initialTags = form.getValues('tags');
		return Array.isArray(initialTags) ? [...initialTags] : [];
	});
	useEffect(() => {
		form.setValue('tags', tags, {shouldDirty: true});
	}, [form, tags]);

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
			<TagListField
				tags={tags}
				onChange={setTags}
				limit={STICKER_TAG_LIMIT}
				disabled={disabled}
				data-flx="expressions.sticker-form.sticker-form-fields.tag-list-field.set-tags"
			/>
		</>
	);
});
