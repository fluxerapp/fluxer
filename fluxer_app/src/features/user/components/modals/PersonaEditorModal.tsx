import * as Modal from '@app/features/app/components/dialogs/Modal';
import { Persona } from '@app/features/personas/models/Persona';
import { i18n } from "@lingui/core";
import { msg, t } from '@lingui/core/macro';
import { observer } from 'mobx-react-lite';
import profileStyles from '@app/features/user/components/modals/tabs/MyProfileTab.module.css';
import { Form } from '@app/features/ui/components/form/Form';
import {useForm, useWatch} from 'react-hook-form';
import { Input } from '@app/features/ui/components/form/FormInput';
import { Trans, useLingui } from '@lingui/react/macro';
import { DISPLAY_NAME_DESCRIPTOR, DOC_I_M_FROM_THE_FUTURE_I_CAME_DESCRIPTOR, PRONOUNS_DESCRIPTOR, WARNING_YOU_HAVE_UNSAVED_CHANGES_PLEASE_SAVE_YOUR_DESCRIPTOR } from './tabs/MyProfileTab';
import { SettingsSection } from '@app/features/app/components/dialogs/shared/SettingsSection';
import styles from '@app/features/user/components/modals/PersonaEditorModal.module.css';
import previewStyles from '@app/features/user/components/profile/ProfilePreview.module.css';
import { clsx } from 'clsx';
import Users from '../../state/Users';
import { ProfilePreview } from '../profile/ProfilePreview';
import { Button } from '@app/features/ui/button/Button';
import { modal } from '@app/features/ui/commands/ModalCommands';
import { Modals } from '@app/features/app/components/dialogs/Modals';
import { useCallback, useEffect, useMemo, useRef, useState, type SetStateAction } from 'react';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import * as PersonaCommands from '@app/features/personas/commands/Personas';
import type { OwnPersonaResponse, PersonaPatchRequest } from '@fluxer/schema/src/domains/persona/PersonaSchemas.js';
import { useFormSubmit } from '@app/features/app/hooks/useFormSubmit';
import { AccentColorPicker } from './tabs/my_profile_tab/AccentColorPicker';
import { useRemoteFormReset } from '@app/lib/forms/RemoteFormReset';
import Personas from '../../state/Personas';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import { DeleteIcon } from '@app/features/ui/action_menu/ContextMenuIcons';
import { ConfirmModal } from '@app/features/app/components/dialogs/ConfirmModal';
import { BioEditor } from './tabs/my_profile_tab/BioEditor';
import type { MentionSegment } from '@app/features/messaging/utils/TextareaSegmentManager';
import type { LexicalRichInputHandle } from '@app/features/lexical/composer/LexicalRichInput';
import type { FlatEmoji } from '@app/features/emoji/types/EmojiTypes';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import { AvatarUploader } from './tabs/my_profile_tab/AvatarUploader';
import { BannerUploader } from './tabs/my_profile_tab/BannerUploader';
import { PersonaProfileCard } from '../profile/PersonaProfile';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import { Accordion } from '@app/features/ui/accordion/Accordion';
import { PlusIcon, WarningCircleIcon } from '@phosphor-icons/react';
import { Tooltip } from '@app/features/ui/tooltip/Tooltip';
import { body } from 'framer-motion/client';
import { useUnsavedChangesFlash } from '../../hooks/useUnsavedChangesFlash';
import { useModalBackHandler } from '@app/features/app/hooks/useModalBackHandler';
import Accessibility from '@app/features/accessibility/state/Accessibility';

const EDIT_PERSONA_DESCRIPTOR = msg({
	message: 'Edit Persona',
	comment: "The title of the persona editor modal."
});
const CREATE_PERSONA_DESCRIPTOR = msg({
	message: 'New Persona',
	comment: "The title of the persona editor modal when it's being used to create a new persona."
});
const INTERNAL_NAME_DESCRIPTOR = msg({
	message: 'Internal Name',
	comment: "Label in the persona editor for a private name for a persona."
});
const TRIGGERS_DESCRIPTOR = msg({
	message: 'Triggers',
	comment: "Label in the persona editor for the message prefixes and suffixes that trigger a persona for a message."
});
const TRIGGER_PREFIX_DESCRIPTOR = msg({
	message: 'Prefix',
	comment: "Label in the persona editor for the message prefixes and suffixes that trigger a persona for a message."
});
const TRIGGER_SUFFIX_DESCRIPTOR = msg({
	message: 'Suffix',
	comment: "Label in the persona editor for the message prefixes and suffixes that trigger a persona for a message."
});
const TRIGGER_MESSAGE_DESCRIPTOR = msg({
	message: 'Message',
	comment: "Label in the persona editor for the message prefixes and suffixes that trigger a persona for a message."
});
const PERSONA_UPDATED_DESCRIPTOR = msg({
	message: 'Persona updated',
	comment: "Short label in the persona editor. Keep it concise."
});
const PERSONA_CREATED_DESCRIPTOR = msg({
	message: 'Persona created',
	comment: "Short label in the persona editor. Keep it concise."
});
const PERSONA_DELETED_DESCRIPTOR = msg({
	message: 'Persona deleted',
	comment: "Short label in the persona editor. Keep it concise."
});

interface PersonaEditorModalProps {
	initialPersona: Persona | null;
}

interface TriggerRowProps {
	prefix: string;
	suffix: string;
	error?: string;
	onPrefixChange: (value: string) => void;
	onSuffixChange: (value: string) => void;
	onDelete: () => void;
}

interface FormInputs {
	avatar?: string | null;
	banner?: string | null;
	bio: string | null;
	internal_name: string | null;
	display_name: string | null;
	pronouns: string | null;
	accent_color: number | null;
	triggers: Persona['triggers'];
}

export const PersonaEditorModal: React.FC<PersonaEditorModalProps> = observer(
	({initialPersona}) => {
		console.log("Using persona", initialPersona, Personas.allPersonasList);
		const [savedPersona, setSavedPersona] = useState(initialPersona);
		const {i18n} = useLingui();
		const user = useMemo(() => Users.currentUser!, []);
		const form = useForm<FormInputs>({
			defaultValues: {...initialPersona, avatar: undefined, banner: undefined},
			resetOptions: {
				keepErrors: false,
			}
		});
		const [bioHydrationKey, setBioHydrationKey] = useState(0);
		const triggers = form.watch("triggers") || [];
		const [hasChangedAvatar, setHasChangedAvatar] = useState(false);
		const [hasChangedBanner, setHasChangedBanner] = useState(false);
		const [flashingUnsavedChanges, setFlashingUnsavedChanges] = useState(false);
		const [flashTrigger, setFlashTrigger] = useState(0);
		const [lastFlashTrigger, setLastFlashTrigger] = useState(0);
		const [ariaAnnouncement, setAriaAnnouncement] = useState('');

		const setTriggers = useCallback((f: (oldValue: Persona['triggers']) => Persona['triggers']) => {
			const newTriggers = f(triggers);
			console.log("jidfusjis", triggers, newTriggers);
			form.setValue("triggers", newTriggers, {
				shouldDirty: true,
				shouldTouch: true
			});
		}, [triggers]);

		const onSubmit = useCallback(
			async (data: FormInputs) => {
				if (savedPersona) {
					const updateData: PersonaPatchRequest = {
						avatar: hasChangedAvatar ? data.avatar || null : undefined,
						banner: hasChangedBanner ? data.banner || null : undefined,
						bio: data.bio,
						internal_name: data.internal_name || undefined,
						display_name: data.display_name || undefined,
						pronouns: data.pronouns,
						accent_color: data.accent_color,
						triggers: data.triggers.map((t) => ({
							prefix: t.prefix || null,
							suffix: t.suffix || null
						}))
					};
					let newPersona: OwnPersonaResponse;
					try {
						newPersona = await PersonaCommands.update(initialPersona!.id, updateData);
					} catch(error: any) {
						console.warn("Could not update persona", error);
						if ('body' in error && 'errors' in error.body && 'path' in error.body.errors[0]) {
							form.clearErrors();
							for (const ferr of error.body.errors) {
								form.setError(ferr.path, {message: ferr.message || "Invalid value"});
							}
						} else if ('body' in error && 'message' in error.body) {
							form.setError("root", {message: error.body.message});
						} else if ('message' in error) {
							form.setError("root", {message: error.message});
						} else {
							form.setError("root", {message: "Something went wrong."});
						}
						return;
					}
					form.reset({...newPersona,
					  triggers: newPersona.triggers.map((v) => ({prefix: v.prefix || undefined, suffix: v.suffix || undefined})),
						avatar: null,
						banner: null
					});
					Personas.cachePersonas([newPersona]);
					ToastCommands.createToast({type: 'success', children: i18n._(PERSONA_UPDATED_DESCRIPTOR)});
					setSavedPersona(new Persona(newPersona));
				} else {
					// Create a persona
					const newPersona = await PersonaCommands.create({
						...form.getValues(),
						internal_name: form.getValues().internal_name!,
					});
					//form.reset({...newPersona, avatar: null, banner: null});
					Personas.cachePersonas([newPersona]);
					ModalCommands.pop();
					ToastCommands.createToast({type: 'success', children: i18n._(PERSONA_CREATED_DESCRIPTOR)});
					setSavedPersona(new Persona(newPersona));
				}
				setHasChangedAvatar(false);
				setHasChangedBanner(false);
			},
			[
				// commitProfileFormValues,
				// isPerGuildProfile,
				// isProfileCustomizationLocked,
				// hasProfileTimezoneAccess,
				// selectedGuildId,
				user,
				initialPersona,
				hasChangedAvatar,
				hasChangedBanner,
				// activeProfileData,
				// avatarAsset,
				// bannerAsset,
				// profileIdentityKey,
			],
		);
		const handleReset = useCallback(() => {
			// TODO: Get new default values, i.e. what was submitted last
			form.reset();
			setBioValue(form.formState.defaultValues?.bio ?? "");
			setBioActualValue(form.formState.defaultValues?.bio ?? "");
			setBioSegments([]);
			setBioHydrationKey((key) => key + 1);
			setHasChangedAvatar(false);
			setHasChangedBanner(false);
		}, [savedPersona]);
		const {handleSubmit: handleSave} = useFormSubmit({
			form,
			onSubmit,
			defaultErrorField: 'internal_name',
		});
		const handleDelete = useCallback(async () => {
			if (!savedPersona) return;
			ModalCommands.push(() => <ConfirmModal
				title="Delete this persona?"
				description={`You are about to delete ${savedPersona.internal_name}. Are you sure this is what you want to do?`}
				onPrimary={async () => {
					await PersonaCommands.deletePersona(savedPersona.id);
					Personas.removePersona(savedPersona.id);
					ToastCommands.createToast({type: 'success', children: i18n._(PERSONA_DELETED_DESCRIPTOR)});
					ModalCommands.popAllByType(PersonaEditorModal);
				}}
				primaryText={<Trans>Delete</Trans>}
				/>)
			}, [savedPersona]);

		const bioComposerRef = useRef<LexicalRichInputHandle | null>(null);
		const [bioValue, setBioValue] = useState(form.formState.defaultValues?.bio ?? "");
		const [bioActualValue, setBioActualValue] = useState(bioValue);
		const [bioSegments, setBioSegments] = useState<Array<MentionSegment>>([]);
		const [bioExpressionPickerOpen, setBioExpressionPickerOpen] = useState(false);
		const handleBioChange = useCallback((display: string, segments: Array<MentionSegment>, wire: string) => {
			const bio = form.getFieldState("bio");
			const ogBio = form.formState.defaultValues?.bio;
			console.log("og bio was:", ogBio);
			console.log("bio was:", bioActualValue);
			console.log("changed bio:", wire);
			if (wire !== bioActualValue) form.setValue("bio", wire, {
				shouldDirty: bio.isDirty || wire !== ogBio,
				shouldTouch: true,
				shouldValidate: true
			});
			setBioValue(display);
			setBioSegments(segments);
			setBioActualValue(wire);
		}, []);
		useWatch({
			control: form.control,
			name: ['bio'],
			compute: ([wireValue]) => {
				if ((wireValue || "") === bioActualValue) return;
				//if (wireValue === null) return;
				setBioValue(wireValue || "");
				setBioSegments([]);
				setBioActualValue(wireValue || "");
			}
		});
		const handleBioEmojiSelect = useCallback((emoji: FlatEmoji, shiftKey?: boolean) => {
			const composer = bioComposerRef.current;
			if (composer == null) {
				return false;
			}
			const didInsert = composer.insertEmoji(emoji);
			if (didInsert && shiftKey !== true) {
				setBioExpressionPickerOpen(false);
			}
			return didInsert;
		}, []);
		const actualBio = bioActualValue;
		const maxBioActualLength = user?.maxBioLength ?? 0;

		const setAvatarHandler = useCallback((b64val: string) => {
			form.setValue("avatar", b64val, {
				shouldDirty: true,
				shouldTouch: true
			});
			setHasChangedAvatar(true);
		}, [savedPersona]);
		const clearAvatarHandler = useCallback(() => {
			form.setValue("avatar", null, {
				shouldDirty: true,
				shouldTouch: true
			});
			setHasChangedAvatar(true);
		}, [savedPersona]);
		const hasAvatar = !!savedPersona?.avatar || !!form.watch("avatar");

		const setBannerHandler = useCallback((b64val: string) => {
			form.setValue("banner", b64val, {
				shouldDirty: true,
				shouldTouch: true
			});
			setHasChangedBanner(true);
		}, [savedPersona]);
		const clearBannerHandler = useCallback(() => {
			form.setValue("banner", null, {
				shouldDirty: true,
				shouldTouch: true
			});
			setHasChangedBanner(true);
		}, [savedPersona]);
		const hasBanner = !!savedPersona?.banner || !!form.watch("banner");

		const TriggerRow = useCallback(({
			onPrefixChange,
			onSuffixChange,
			onDelete,
			error,
			prefix,
			suffix,
			...props
		}: React.ComponentPropsWithRef<'div'> & TriggerRowProps) => {
			return <div className={styles.triggerRowWrapper}>
				<div {...props} className={styles.triggerRow}>
					<Input
						className={styles.triggerPrefix}
						placeholder={i18n._(TRIGGER_PREFIX_DESCRIPTOR)}
						value={prefix}
						onChange={(e) => onPrefixChange(e.target.value)}
					/>
					<Input
						className={styles.triggerMessagePlaceholder}
						disabled
						value={i18n._(TRIGGER_MESSAGE_DESCRIPTOR)}
					/>
					<Input
						className={styles.triggerSuffix}
						placeholder={i18n._(TRIGGER_SUFFIX_DESCRIPTOR)}
						value={suffix}
						onChange={(e) => onSuffixChange(e.target.value)}
					/>
					<Tooltip text="Delete">
						<Button
							className={styles.triggerDelete}
							variant="danger"
							square
							icon={<DeleteIcon />}
							aria-label="Delete"
							onClick={onDelete}
						/>
					</Tooltip>
				</div>
				{error && <div className={styles.error}><WarningCircleIcon size={24} /><span>{error}</span></div>}
			</div>
		}, []);

		const prefersReducedMotion = Accessibility.useReducedMotion;
		useEffect(() => {
			if (flashTrigger > lastFlashTrigger) {
				setLastFlashTrigger(flashTrigger);
				setAriaAnnouncement(i18n._(WARNING_YOU_HAVE_UNSAVED_CHANGES_PLEASE_SAVE_YOUR_DESCRIPTOR));
				setTimeout(() => {
					setAriaAnnouncement('');
				}, 1000);
			}
		}, [flashTrigger, lastFlashTrigger]);
		useEffect(() => {
			if (flashTrigger > lastFlashTrigger && !flashingUnsavedChanges) {
				setFlashingUnsavedChanges(true);
				setTimeout(() => {
					setFlashingUnsavedChanges(false);
				}, 200);
			}
		}, [flashTrigger, lastFlashTrigger]);
		const handleClose = useCallback(() => {
			if (form.formState.isDirty) {
				setFlashTrigger((f) => f+1);
				return;
			}
			ModalCommands.pop();
		}, [setFlashTrigger, form.formState.isDirty]);

		return <Modal.Root size={"large"} onClose={handleClose}>
			<output
				aria-live="assertive"
				aria-atomic="true"
				className={profileStyles.srOnly}
				data-flx="user.persona-editor-modal.sr-only"
			>
				{ariaAnnouncement}
			</output>
			<Modal.Header onClose={handleClose} title={initialPersona ? i18n._(EDIT_PERSONA_DESCRIPTOR) : i18n._(CREATE_PERSONA_DESCRIPTOR)} />
			<Modal.Content>
				<Form
					form={form}
					onSubmit={onSubmit}
					data-flx="user.persona-editor-modal.form.submit"
				>
					<div
						className={clsx(profileStyles.contentLayout)}
						data-flx="user.persona-editor-modal.content-layout"
					>
						<div className={profileStyles.formColumn} data-flx="user.persona-editor-modal.form-column">
							{form.formState.errors.root?.message && <div className={styles.error}><WarningCircleIcon size={24} /><span>{form.formState.errors.root?.message}</span></div>}
							<Input
								data-flx="user.persona-editor-modal.input--internal-name"
								label={i18n._(INTERNAL_NAME_DESCRIPTOR)}
								footer={<span className={styles.footerHintText}><Trans>This name stays private, and only appears in lists of your own personas.</Trans></span>}
								{...form.register('internal_name')}
								value={form.watch('internal_name') || ''}
								required
							/>
							<Input
								data-flx="user.persona-editor-modal.input--display-name"
								label={i18n._(DISPLAY_NAME_DESCRIPTOR)}
								{...form.register('display_name')}
								value={form.watch('display_name') || ''}
							/>
							<Input
								data-flx="user.persona-editor-modal.input--pronouns"
								label={i18n._(PRONOUNS_DESCRIPTOR)}
								{...form.register('pronouns')}
								value={form.watch('pronouns') || ''}
							/>
							<div data-flx="user.persona-editor-modal.avatar-uploader-outer">
								<AvatarUploader
									hasAvatar={hasAvatar}
									requireAnimatedAvatarEntitlement={true}
									onAvatarChange={setAvatarHandler}
									onAvatarClear={clearAvatarHandler}
									//disabled={isProfileCustomizationLocked || isPerGuildProfileCustomizationDisabled}
									//disableModeSelection={isProfileCustomizationLocked}
									isPerGuildProfile={false}
									errorMessage={form.formState.errors.avatar?.message}
									avatarMode={hasAvatar ? "custom" : "unset"}
									//onAvatarModeChange={handleAvatarModeChange}
									data-flx="user.persona-editor-modal.avatar-uploader"
								/>
							</div>
							<div data-flx="user.persona-editor-modal.banner-upload-outer">
								<BannerUploader
									hasBanner={hasBanner}
									onBannerChange={setBannerHandler}
									onBannerClear={clearBannerHandler}
									requireBannerEntitlement={true}
									hideUploadWhenMissingEntitlement={true}
									isPerGuildProfile={false}
									errorMessage={form.formState.errors.banner?.message}
									bannerMode={hasBanner ? "custom" : "unset"}
									//onBannerModeChange={handleBannerModeChange}
									data-flx="user.persona-editor-modal.banner-uploader"
								/>
							</div>
							<div
								// className={isPerGuildProfile && !hasPerGuildProfiles ? styles.opacityHalf : ''}
								// data-flx="user.persona-editor-modal.opacity-half"
							>
								<AccentColorPicker
									value={form.watch('accent_color') ?? null}
									onChange={(value: number | null) => form.setValue('accent_color', value, {shouldDirty: true})}
									//disabled={isProfileCustomizationLocked || isPerGuildProfileCustomizationDisabled}
									errorMessage={form.formState.errors.accent_color?.message}
									data-flx="user.persona-editor-modal.accent-color-picker.set-value"
								/>
							</div>
							<div
								//className={isPerGuildProfile && !hasPerGuildProfiles ? styles.opacityHalf : ''}
								//data-flx="user.my-profile-tab.my-profile-tab-component.opacity-half--2"
							>
								<BioEditor
									{...form.register('bio')}
									initialValue={bioValue}
									initialSegments={bioSegments}
									hydrationKey={bioHydrationKey}
									onChange={handleBioChange}
									onEmojiSelect={handleBioEmojiSelect}
									placeholder={
										i18n._(DOC_I_M_FROM_THE_FUTURE_I_CAME_DESCRIPTOR)
									}
									actualLength={actualBio.length}
									actualMaxLength={maxBioActualLength}
									disabled={false}
									// disabled={isProfileCustomizationLocked || isPerGuildProfileCustomizationDisabled}
									isMobile={MobileLayout.enabled}
									errorMessage={
										form.formState.errors.bio != null && form.formState.errors.bio.message != null
											? form.formState.errors.bio.message
											: null
									}
									composerRef={bioComposerRef}
									emojiPickerOpen={bioExpressionPickerOpen}
									onEmojiPickerOpenChange={setBioExpressionPickerOpen}
									data-flx="user.persona-editor-modal.bio-editor.bio-change"
								/>
							</div>
							<Accordion
								id="persona-editor-triggers"
								defaultExpanded
								headerAction={<Button
									variant="primary"
									leftIcon={<PlusIcon />}
									onClick={() => {
										if (triggers.find((v) => v.prefix === undefined && v.suffix === undefined)) return;
										setTriggers((t) => [{}, ...t])}
									}
								><Trans>Add trigger</Trans></Button>}
								title={i18n._(TRIGGERS_DESCRIPTOR)}>
									{form.getFieldState("triggers").error?.message && <div className={styles.error}><WarningCircleIcon size={24} /><span>{form.getFieldState("triggers").error?.message}</span></div>}
									{/* <p><Trans>Set prefix-suffix pairs that you can put on messages to use this persona for that message. Each prefix and suffix in a pair must be used together. You cannot leave both prefix and suffix empty, but neither are required.</Trans></p> */}
								{triggers?.map((trigger, i) => <TriggerRow
									key={i}
									prefix={trigger.prefix || ""}
									suffix={trigger.suffix || ""}
									onDelete={() => {
										setTriggers((t) => t.filter((_,ti) => ti !== i))
										form.clearErrors();
									}}
									error={
										form.getFieldState(`triggers.${i}.prefix`).error?.message
										|| form.getFieldState(`triggers.${i}.suffix`).error?.message
										|| form.getFieldState(`triggers.${i}`).error?.message
									}
									onPrefixChange={(value) => setTriggers((t) => t.map((v,ti) => {
										if (ti !== i) return v;
										return {
											...v,
											prefix: value
										}
									}))}
									onSuffixChange={(value) => setTriggers((t) => t.map((v,ti) => {
										if (ti !== i) return v;
										return {
											...v,
											suffix: value
										}
									}))}
								/>)}
							</Accordion>
						</div>
						<div
							className={profileStyles.previewColumn}
							data-flx="user.persona-editor-modal.preview-column"
						>
							{/* <ProfilePreview
								user={user}
							/> */}
							<FocusRing offset={-2} data-flx="user.profile.profile-preview.focus-ring">
								<div
									className={previewStyles.previewInteractive}
									role="group"
									//aria-label={i18n._(PROFILE_PREVIEW_PRESS_ENTER_TO_OPEN_FULL_PREVIEW_DESCRIPTOR)}
									onKeyDown={/*handlePreviewKeyDown*/() => {}}
									data-flx="user.profile.profile-preview.preview-interactive.preview-key-down"
								>
									{savedPersona && <PersonaProfileCard
										user={user}
										persona={savedPersona!}
										showPreviewLabel
									/>}
								</div>
							</FocusRing>
						</div>
					</div>
				</Form>
			</Modal.Content>
			<Modal.FormFooter className={clsx(styles.unsavedChangesTransition, styles.unsavedChangesArea, flashingUnsavedChanges && styles.flashingUnsavedChanges, prefersReducedMotion && styles.prefersReducedMotion)}>
				{form.formState.isDirty && <span className={styles.unsavedChangesWarning}><Trans>You have unsaved changes.</Trans></span>}
				{!form.formState.isDirty && initialPersona && <Button
					variant="danger"
					className={styles.deleteButton}
					leftIcon={<DeleteIcon />}
					disabled={form.formState.isLoading}
					onClick={handleDelete}
				><Trans>Delete</Trans></Button>}
				<Button variant="secondary" disabled={form.formState.isLoading /* || !form.formState.isValid */ || !form.formState.isDirty} onClick={handleReset}>Reset</Button>
				<Button variant="primary" disabled={form.formState.isLoading /* || !form.formState.isValid */ || !form.formState.isDirty} onClick={handleSave}>Save</Button>
			</Modal.FormFooter>
		</Modal.Root>;
	});
