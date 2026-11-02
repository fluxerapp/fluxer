// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {OPERATOR_PASS_NAME, PRODUCT_LABS_COMMUNITY_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import {OpenOperatorsButton} from '@app/features/premium/components/operator_pass/OpenOperatorsButton';
import {OperatorPassCertificateNamesField} from '@app/features/premium/components/operator_pass/OperatorPassCertificateNamesField';
import {OperatorPassConsentBox} from '@app/features/premium/components/operator_pass/OperatorPassConsentBox';
import {
	asksOperatorPassLabsGreeting,
	getStoredOperatorPassLabsGreeting,
	OperatorPassLabsGreetingField,
} from '@app/features/premium/components/operator_pass/OperatorPassLabsGreetingField';
import styles from '@app/features/premium/components/operator_pass/OperatorPassOfferModal.module.css';
import {DOWNLOAD_CERTIFICATE_DESCRIPTOR} from '@app/features/premium/components/operator_pass/OperatorPassPurchasesSection';
import {OperatorPassWallNameField} from '@app/features/premium/components/operator_pass/OperatorPassWallNameField';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {
	focusFirstInvalidOperatorPassField,
	focusOperatorPassField,
	formatOperatorPassLongDate,
	getOperatorPassCertificateNameProblem,
	getOperatorPassErrorCode,
	getOperatorPassWallNameProblem,
	normalizeOperatorPassCertificateName,
	normalizeOperatorPassWallName,
	OPERATOR_PASS_VISIONARY_DATE_TIME_ZONE,
	type OperatorPassCertificateNameProblem,
	type OperatorPassWallNameProblem,
} from '@app/features/premium/utils/OperatorPassUtils';
import {OPERATOR_PASS_AGE_CONFIRMATION_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import Users from '@app/features/user/state/Users';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {
	OPERATOR_PASS_VISIONARY_BOX_VERSION,
	truncateOperatorPassCertificateName,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

const TITLE_DESCRIPTOR = msg({
	message: 'Turn on your Operator Pass',
	comment:
		'Title of the dialog where a Visionary turns on the Operator Pass that comes with Visionary. Operator Pass and Visionary are product names, keep them as is.',
});
const DATE_LINE_DESCRIPTOR = msg({
	message: 'Your certificate shows the name below and {date}, the date you got Visionary.',
	comment:
		'First line in the dialog where a Visionary turns on the Operator Pass that comes with Visionary. {date} is a long date such as November 20, 2026. Never translate certificate as diploma, card, license or token.',
});
const NAME_HINT_DESCRIPTOR = msg({
	message: 'We print the name exactly as you type it.',
	comment:
		'Hint under the name field for the Operator certificate when a Visionary turns on the Operator Pass that comes with Visionary. The certificate can print names in any writing system, such as Latin, Arabic, Chinese or Devanagari.',
});
const SHOW_BADGE_DESCRIPTOR = msg({
	message: 'Show the Operator badge on my profile',
	comment:
		'Switch in the dialog where a Visionary turns on the Operator Pass that comes with Visionary. Off by default. Operator is the badge name, keep it as is.',
});
const LABS_LINE_DESCRIPTOR = msg({
	message:
		'Your Visionary gives you #operators in {labsName} once you join with this account. After you turn on your Operator Pass, joining also gives you the Operator role.',
	comment:
		'Line in the dialog where a Visionary turns on the Operator Pass that comes with Visionary, shown when the account is not in {labsName}, the community name. Operator is a role name and #operators a channel name, keep both as is.',
});
const TURNED_ON_DESCRIPTOR = msg({
	message: 'Your Operator Pass is on. Your certificate is ready to download.',
	comment:
		'Shown in the dialog after a Visionary turned on the Operator Pass that comes with Visionary, above the button that downloads the certificate PDF.',
});
const TURN_ON_FAILED_DESCRIPTOR = msg({
	message: "Couldn't turn on the pass. Try again.",
	comment: 'Toast when turning on the Operator Pass that comes with Visionary failed.',
});
export const OPERATOR_PASS_TURN_ON_DESCRIPTOR = msg({
	message: 'Turn on',
	comment:
		'Button that turns on the Operator Pass that comes with Visionary, in profile settings and at the bottom of its dialog. Nothing is bought.',
});

const logger = new Logger('OperatorPassVisionaryModal');

const VISIONARY_MODAL_KEY = 'operator-pass-visionary';
const NAME_FIELD_ID = 'operator-pass-visionary-certificate-name';
const LABS_GREETING_FIELD_ID = 'operator-pass-visionary-labs-greeting';
const WALL_FIELD_ID = 'operator-pass-visionary-wall-name';
const TERMS_BOX_ID = 'operator-pass-visionary-terms';
const AGE_BOX_ID = 'operator-pass-visionary-age';

const OperatorPassVisionaryModal = observer(function OperatorPassVisionaryModal() {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	const visionary = OperatorPassHoldings.passes?.visionary ?? null;
	const [name, setName] = useState(() =>
		truncateOperatorPassCertificateName(normalizeOperatorPassCertificateName(user?.displayName ?? '')),
	);
	const [nameProblem, setNameProblem] = useState<OperatorPassCertificateNameProblem | null>(null);
	const [showBadge, setShowBadge] = useState(false);
	const [wallName, setWallName] = useState(() => user?.displayName ?? '');
	const [wallOptOut, setWallOptOut] = useState(true);
	const [wallProblem, setWallProblem] = useState<OperatorPassWallNameProblem | null>(null);
	const [labsGreeting, setLabsGreeting] = useState(getStoredOperatorPassLabsGreeting);
	const [terms, setTerms] = useState(false);
	const [termsMissing, setTermsMissing] = useState(false);
	const [age, setAge] = useState(false);
	const [ageMissing, setAgeMissing] = useState(false);
	const [submitting, setSubmitting] = useState(false);
	const [turnedOn, setTurnedOn] = useState(false);
	const [hadBadge] = useState(
		() =>
			(OperatorPassHoldings.passes?.passes.some((pass) => pass.source !== 'visionary') ?? false) &&
			!(user?.hiddenProfileBadges ?? []).includes('operator_pass'),
	);
	const [hadWallChoice, setHadWallChoice] = useState(false);
	useEffect(() => {
		let active = true;
		OperatorPassCommands.fetchState()
			.then((state) => {
				if (active) setHadWallChoice(state.holder?.wall_chosen ?? false);
			})
			.catch((error) => logger.warn('Failed to load the Operator Pass state for turning on the Visionary pass', error));
		return () => {
			active = false;
		};
	}, []);
	if (!visionary) return null;
	const labsMember = visionary.labs_member;
	const asksLabsGreeting = asksOperatorPassLabsGreeting();
	const handleError = (error: unknown) => {
		logger.error('Turning on the Visionary Operator Pass failed', error);
		switch (getOperatorPassErrorCode(error)) {
			case APIErrorCodes.OPERATOR_PASS_CERTIFICATE_NAME_REJECTED:
				setNameProblem('rejected');
				focusOperatorPassField(NAME_FIELD_ID);
				return;
			case APIErrorCodes.OPERATOR_PASS_WALL_NAME_REJECTED:
				setWallProblem('rejected');
				focusOperatorPassField(WALL_FIELD_ID);
				return;
			case APIErrorCodes.OPERATOR_PASS_TERMS_NOT_ACCEPTED:
				setTerms(false);
				setTermsMissing(true);
				focusOperatorPassField(TERMS_BOX_ID);
				return;
			case APIErrorCodes.OPERATOR_PASS_AGE_CONFIRMATION_REQUIRED:
				setAge(false);
				setAgeMissing(true);
				focusOperatorPassField(AGE_BOX_ID);
				return;
			default:
				ToastCommands.error(i18n._(TURN_ON_FAILED_DESCRIPTOR));
		}
	};
	const handleTurnOn = async () => {
		if (submitting) return;
		const certificateName = normalizeOperatorPassCertificateName(name);
		const certificateProblem = getOperatorPassCertificateNameProblem(certificateName);
		const asksWall = !hadWallChoice;
		const wallNameProblem = asksWall ? getOperatorPassWallNameProblem(wallName, wallOptOut) : null;
		setNameProblem(certificateProblem);
		setWallProblem(wallNameProblem);
		setTermsMissing(!terms);
		setAgeMissing(!age);
		if (
			focusFirstInvalidOperatorPassField([
				{id: NAME_FIELD_ID, invalid: certificateProblem !== null},
				{id: WALL_FIELD_ID, invalid: wallNameProblem !== null},
				{id: TERMS_BOX_ID, invalid: !terms},
				{id: AGE_BOX_ID, invalid: !age},
			])
		) {
			return;
		}
		setSubmitting(true);
		try {
			await OperatorPassCommands.turnOnVisionaryPass({
				certificate_name: certificateName,
				show_badge: hadBadge || showBadge,
				wall:
					!asksWall || wallOptOut
						? {opted_out: true}
						: {opted_out: false, name: normalizeOperatorPassWallName(wallName)},
				...(asksLabsGreeting ? {labs_greeting: labsGreeting} : {}),
				terms_accepted: true,
				age_confirmed: true,
				terms_version: visionary.terms_version,
				box_text_version: OPERATOR_PASS_VISIONARY_BOX_VERSION,
				consent_locale: i18n.locale,
			});
			setTurnedOn(true);
		} catch (error) {
			handleError(error);
		} finally {
			setSubmitting(false);
		}
	};
	const certificateUrl = visionary.certificate_url;
	if (turnedOn || visionary.state === 'on') {
		return (
			<Modal.Root size="small" centered data-flx="premium.operator-pass-visionary-modal.modal-root">
				<Modal.Header title={i18n._(TITLE_DESCRIPTOR)} data-flx="premium.operator-pass-visionary-modal.modal-header" />
				<Modal.Content data-flx="premium.operator-pass-visionary-modal.modal-content">
					<Modal.ContentLayout data-flx="premium.operator-pass-visionary-modal.content-layout">
						<Modal.Description data-flx="premium.operator-pass-visionary-modal.turned-on">
							{i18n._(TURNED_ON_DESCRIPTOR)}
						</Modal.Description>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="premium.operator-pass-visionary-modal.modal-footer">
					<OpenOperatorsButton
						variant="secondary"
						onOpened={() => ModalCommands.popWithKey(VISIONARY_MODAL_KEY)}
						data-flx="premium.operator-pass-visionary-modal.button.operators"
					/>
					{certificateUrl && (
						<Button
							variant="primary"
							onClick={() => void openExternalUrl(certificateUrl)}
							data-flx="premium.operator-pass-visionary-modal.button.download-certificate"
						>
							{i18n._(DOWNLOAD_CERTIFICATE_DESCRIPTOR)}
						</Button>
					)}
				</Modal.Footer>
			</Modal.Root>
		);
	}
	return (
		<Modal.Root size="small" centered data-flx="premium.operator-pass-visionary-modal.modal-root">
			<Modal.Header title={i18n._(TITLE_DESCRIPTOR)} data-flx="premium.operator-pass-visionary-modal.modal-header" />
			<Modal.Content data-flx="premium.operator-pass-visionary-modal.modal-content">
				<Modal.ContentLayout data-flx="premium.operator-pass-visionary-modal.content-layout">
					<Modal.Description data-flx="premium.operator-pass-visionary-modal.date-line">
						{i18n._(DATE_LINE_DESCRIPTOR, {
							date: formatOperatorPassLongDate(visionary.since, OPERATOR_PASS_VISIONARY_DATE_TIME_ZONE),
						})}
					</Modal.Description>
					<OperatorPassCertificateNamesField
						id={NAME_FIELD_ID}
						names={[name]}
						quantity={1}
						sameName
						problem={nameProblem}
						problemIndex={null}
						disabled={submitting}
						hint={i18n._(NAME_HINT_DESCRIPTOR)}
						onNameChange={(_index, value) => {
							setName(value);
							if (nameProblem) setNameProblem(getOperatorPassCertificateNameProblem(value));
						}}
						onSameNameChange={() => {}}
						data-flx="premium.operator-pass-visionary-modal.certificate-name"
					/>
					{!hadBadge && (
						<Switch
							label={i18n._(SHOW_BADGE_DESCRIPTOR)}
							value={showBadge}
							onChange={setShowBadge}
							disabled={submitting}
							data-flx="premium.operator-pass-visionary-modal.switch.show-badge"
						/>
					)}
					{!hadWallChoice && (
						<OperatorPassWallNameField
							id={WALL_FIELD_ID}
							name={wallName}
							optOut={wallOptOut}
							problem={wallProblem}
							disabled={submitting}
							context="checkout"
							onNameChange={(value) => {
								setWallName(value);
								if (wallProblem) setWallProblem(getOperatorPassWallNameProblem(value, wallOptOut));
							}}
							onOptOutChange={(value) => {
								setWallOptOut(value);
								if (value) setWallProblem(null);
							}}
							data-flx="premium.operator-pass-visionary-modal.wall-name"
						/>
					)}
					{asksLabsGreeting ? (
						<OperatorPassLabsGreetingField
							id={LABS_GREETING_FIELD_ID}
							checked={labsGreeting}
							disabled={submitting}
							onChange={setLabsGreeting}
							variant="visionary"
							data-flx="premium.operator-pass-visionary-modal.labs-greeting"
						/>
					) : (
						!labsMember && (
							<p className={styles.note} data-flx="premium.operator-pass-visionary-modal.labs-line">
								{i18n._(LABS_LINE_DESCRIPTOR, {labsName: PRODUCT_LABS_COMMUNITY_NAME})}
							</p>
						)
					)}
					<OperatorPassConsentBox
						id={TERMS_BOX_ID}
						checked={terms}
						missing={termsMissing}
						disabled={submitting}
						onChange={(value) => {
							setTerms(value);
							if (value) setTermsMissing(false);
						}}
						data-flx="premium.operator-pass-visionary-modal.terms"
					>
						<Trans comment="Required checkbox in the dialog where a Visionary turns on the Operator Pass that comes with Visionary. Consumer law text, translate faithfully and keep every condition. The linked text names the Operator Pass terms document. {OPERATOR_PASS_NAME} is a product name and stays untranslated, and so does Visionary. Passed on means given to another account with the Pass it on feature.">
							I accept the{' '}
							<ExternalLink
								href={`${Routes.operatorPassTerms()}#13-visionary`}
								data-flx="premium.operator-pass-visionary-modal.terms-link"
							>
								{OPERATOR_PASS_NAME} terms
							</ExternalLink>
							. This pass is part of your Visionary, so it can't be passed on.
						</Trans>
					</OperatorPassConsentBox>
					<OperatorPassConsentBox
						id={AGE_BOX_ID}
						checked={age}
						missing={ageMissing}
						disabled={submitting}
						onChange={(value) => {
							setAge(value);
							if (value) setAgeMissing(false);
						}}
						data-flx="premium.operator-pass-visionary-modal.age"
					>
						{i18n._(OPERATOR_PASS_AGE_CONFIRMATION_DESCRIPTOR)}
					</OperatorPassConsentBox>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="premium.operator-pass-visionary-modal.modal-footer">
				<Button
					variant="primary"
					onClick={() => void handleTurnOn()}
					submitting={submitting}
					data-flx="premium.operator-pass-visionary-modal.button.turn-on"
				>
					{i18n._(OPERATOR_PASS_TURN_ON_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});

export function openOperatorPassVisionaryModal(): void {
	ModalCommands.pushWithKey(
		modal(() => <OperatorPassVisionaryModal data-flx="premium.operator-pass-visionary-modal.open" />),
		VISIONARY_MODAL_KEY,
	);
}
