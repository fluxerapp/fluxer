import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import styles from '@app/features/guild/components/modals/AddGuildModal.module.css';
import {
	renderGuildCreateAccountGate,
	renderGuildCreateFields,
	useGuildCreateIconField,
} from '@app/features/guild/components/modals/add_guild_modal/GuildCreateShared';
import {
	CREATE_COMMUNITY_FORM_DESCRIPTOR,
	type GuildCreateFormInputs,
	handleGuildCreationError,
	ModalFooterContext,
} from '@app/features/guild/components/modals/add_guild_modal/shared';
import {CREATE_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useContext, useEffect, useId} from 'react';
import {useForm} from 'react-hook-form';

const IMAGE_COULDN_T_BE_USED_DESCRIPTOR = msg({
	message: "Image couldn't be used",
	comment: 'Error modal title shown when a create-community icon upload cannot be accepted or processed.',
});
const CHANGE_ICON_DESCRIPTOR = msg({
	message: 'Change icon',
	comment:
		'Title of the modal where the user picks a create-community icon source. Keep it concise. Keep the tone plain and specific.',
});

const ICON_FIELD_MESSAGES = {
	imageCouldNotBeUsed: IMAGE_COULDN_T_BE_USED_DESCRIPTOR,
	changeIcon: CHANGE_ICON_DESCRIPTOR,
};

export const GuildCreateForm = observer(() => {
	const {i18n} = useLingui();
	const form = useForm<GuildCreateFormInputs>({defaultValues: {name: ''}});
	const modalFooterContext = useContext(ModalFooterContext);
	const formId = useId();
	const nameValue = form.watch('name');
	const iconField = useGuildCreateIconField({
		form: form,
		nameValue,
		dataFlx: 'guild.add-guild-modal.guild-create-form',
		cropModalDataFlx: 'guild.add-guild-modal.handle-icon-upload.asset-crop-modal',
		clearIconErrors: true,
		messages: ICON_FIELD_MESSAGES,
	});
	const onSubmit = useCallback(async (data: GuildCreateFormInputs) => {
		try {
			const guild = await GuildCommands.create({icon: data.icon, name: data.name});
			ModalCommands.pop();
			NavigationCommands.selectChannel(guild.id, guild.system_channel_id || undefined);
		} catch (error) {
			handleGuildCreationError(error);
		}
	}, []);
	const {handleSubmit, isSubmitting} = useFormSubmit({form, onSubmit, defaultErrorField: 'name'});
	useEffect(() => {
		const isNameEmpty = !nameValue?.trim();
		modalFooterContext?.setFooterContent(
			<>
				<Button
					onClick={modalFooterContext.onBack}
					variant="secondary"
					data-flx="guild.add-guild-modal.guild-create-form.button.back"
				>
					<Trans>Back</Trans>
				</Button>
				<Button
					onClick={handleSubmit}
					submitting={isSubmitting}
					disabled={isNameEmpty}
					data-flx="guild.add-guild-modal.guild-create-form.button.submit"
				>
					{i18n._(CREATE_COMMUNITY_DESCRIPTOR)}
				</Button>
			</>,
		);
		return () => modalFooterContext?.setFooterContent(null);
	}, [handleSubmit, isSubmitting, modalFooterContext, nameValue]);
	const accountGate = renderGuildCreateAccountGate(i18n, 'guild.add-guild-modal.guild-create-form');
	if (accountGate) {
		return accountGate;
	}
	return (
		<div className={styles.formContainer} data-flx="guild.add-guild-modal.guild-create-form.form-container--3">
			<Modal.Description data-flx="guild.add-guild-modal.guild-create-form.modal-description">
				<Trans>Create a community for you and your friends to chat.</Trans>
			</Modal.Description>
			<Form
				form={form}
				onSubmit={handleSubmit}
				id={formId}
				aria-label={i18n._(CREATE_COMMUNITY_FORM_DESCRIPTOR)}
				data-flx="guild.add-guild-modal.guild-create-form.form.submit"
			>
				{renderGuildCreateFields({
					...iconField,
					i18n,
					form: form,
					dataFlx: 'guild.add-guild-modal.guild-create-form',
					iconSectionDataFlx: 'guild.add-guild-modal.guild-create-form.icon-section',
					nameInputDataFlx: 'guild.add-guild-modal.guild-create-form.input.text',
					showIconError: true,
				})}
			</Form>
		</div>
	);
});
