// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/guild/components/modals/AddGuildModal.module.css';
import {
	type GuildJoinFormInputs,
	INVITE_LINK_DESCRIPTOR,
	JOIN_COMMUNITY_FORM_DESCRIPTOR,
	ModalFooterContext,
} from '@app/features/guild/components/modals/add_guild_modal/shared';
import {JOIN_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import {currentInstanceTarget} from '@app/features/platform/transport/InstanceHTTP';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useContext, useEffect, useId, useMemo} from 'react';
import {useForm} from 'react-hook-form';

export const GuildJoinForm = observer(() => {
	const {i18n} = useLingui();
	const form = useForm<GuildJoinFormInputs>({defaultValues: {code: ''}});
	const modalFooterContext = useContext(ModalFooterContext);
	const formId = useId();
	const randomInviteCode = useMemo(() => {
		const chars = 'abcdefghijklmnopqrstuvwxyz0123456789';
		const length = Math.floor(Math.random() * 7) + 6;
		let result = '';
		for (let i = 0; i < length; i++) {
			result += chars.charAt(Math.floor(Math.random() * chars.length));
		}
		return result;
	}, []);
	const onSubmit = useCallback(
		async (data: GuildJoinFormInputs) => {
			const parsedCode = InviteUtils.findInvite(data.code) ?? data.code;
			const target = currentInstanceTarget();
			const invite = await InviteCommands.fetch(parsedCode, target);
			await InviteCommands.acceptAndTransitionToChannel(invite.code, i18n, target);
			ModalCommands.pop();
		},
		[i18n],
	);
	const {handleSubmit, isSubmitting} = useFormSubmit({form, onSubmit, defaultErrorField: 'code'});
	const codeValue = form.watch('code');
	useEffect(() => {
		const isCodeEmpty = !codeValue?.trim();
		modalFooterContext?.setFooterContent(
			<>
				<Button
					onClick={modalFooterContext.onBack}
					variant="secondary"
					data-flx="guild.add-guild-modal.guild-join-form.button.back"
				>
					<Trans>Back</Trans>
				</Button>
				<Button
					onClick={handleSubmit}
					submitting={isSubmitting}
					disabled={isCodeEmpty}
					data-flx="guild.add-guild-modal.guild-join-form.button.submit"
				>
					{i18n._(JOIN_COMMUNITY_DESCRIPTOR)}
				</Button>
			</>,
		);
		return () => modalFooterContext?.setFooterContent(null);
	}, [handleSubmit, isSubmitting, modalFooterContext, codeValue]);
	return (
		<div className={styles.formContainer} data-flx="guild.add-guild-modal.guild-join-form.form-container">
			<Modal.Description data-flx="guild.add-guild-modal.guild-join-form.modal-description">
				<Trans>Enter the invite link to join a community.</Trans>
			</Modal.Description>
			<Form
				form={form}
				onSubmit={handleSubmit}
				id={formId}
				aria-label={i18n._(JOIN_COMMUNITY_FORM_DESCRIPTOR)}
				data-flx="guild.add-guild-modal.guild-join-form.form.submit"
			>
				<div className={styles.iconSection} data-flx="guild.add-guild-modal.guild-join-form.icon-section">
					<Input
						data-flx="guild.add-guild-modal.guild-join-form.input.text"
						{...form.register('code')}
						autoFocus={true}
						error={form.formState.errors.code?.message}
						label={i18n._(INVITE_LINK_DESCRIPTOR)}
						minLength={1}
						maxLength={100}
						name="code"
						placeholder={`${RuntimeConfig.inviteEndpoint}/${randomInviteCode}`}
						required={true}
						type="text"
					/>
				</div>
			</Form>
		</div>
	);
});
