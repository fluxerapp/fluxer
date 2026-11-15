// SPDX-License-Identifier: AGPL-3.0-or-later

import {TagListField} from '@app/features/expressions/components/modals/TagListField';
import {Input, Textarea} from '@app/features/ui/components/form/FormInput';
import Users from '@app/features/user/state/Users';
import {MAX_FAVORITE_MEME_TAGS} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';
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

interface MemeFormFieldsProps {
	form: UseFormReturn<{
		name: string;
		altText?: string;
		tags: Array<string>;
	}>;
}

export const MemeFormFields = observer(function MemeFormFields({form}: MemeFormFieldsProps) {
	const {i18n} = useLingui();

	const [tags, setTags] = useState<Array<string>>(() => {
		const initialTags = form.getValues('tags');
		return Array.isArray(initialTags) ? [...initialTags] : [];
	});
	const currentUser = Users.getCurrentUser();
	const tagLimit = currentUser?.maxFavoriteMemeTags ?? MAX_FAVORITE_MEME_TAGS;
	useEffect(() => {
		form.setValue('tags', tags, {shouldDirty: true});
	}, [tags, form]);

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
			/>
			<TagListField
				tags={tags}
				onChange={setTags}
				limit={tagLimit}
				data-flx="expressions.meme-form.meme-form-fields.tag-list-field.set-tags"
			/>
		</>
	);
});
