// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {PRODUCT_LABS_COMMUNITY_NAME} from '@app/features/app/config/I18nDisplayConstants';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import {OpenOperatorsButton} from '@app/features/premium/components/operator_pass/OpenOperatorsButton';
import {
	asksOperatorPassLabsGreeting,
	LABS_GREETING_LABEL_DESCRIPTOR,
} from '@app/features/premium/components/operator_pass/OperatorPassLabsGreetingField';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {showsOperatorPassLabsSettings} from '@app/features/premium/utils/BillingTabVisibility';
import {WALL_SAVE_FAILED_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import styles from '@app/features/user/components/modals/tabs/operator_pass_tab/OperatorPassWallSettings.module.css';
import Users from '@app/features/user/state/Users';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const LABS_GREETING_SETTING_HINT_DESCRIPTOR = msg({
	message: 'We welcome you by name in #operators once, soon after your pass is delivered or you turn this on.',
	comment:
		'Description under the switch "Welcome me in #operators" in the Operator Pass settings. #operators is a channel in the Fluxer Labs community, keep it as is. Turning the switch on after the pass is delivered still queues the one welcome.',
});
const LABS_SECTION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'The Operator role opens #operators in {labsName}, where you can talk about self-hosting, bots and the API.',
	comment:
		'Description of the Fluxer Labs section in the Operator Pass settings. #operators is a channel name, keep it as is. {labsName} is the name of the community, such as Fluxer Labs.',
});

const LABS_NOT_MEMBER_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"You're not in {labsName}. Joining gives you the Operator role, which opens #operators, where you can talk about self-hosting, bots and the API.",
	comment:
		'Description of the Fluxer Labs section in the Operator Pass settings when the account is not a member of the community. A button "Join {labsName}" follows it. #operators is a channel name and Operator is the role name, keep both as is. {labsName} is the name of the community, such as Fluxer Labs.',
});
const JOIN_LABS_DESCRIPTOR = msg({
	message: 'Join {labsName}',
	comment:
		'Button in the Fluxer Labs section of the Operator Pass settings when the account is not a member of the community. It adds the account to the community and opens #operators. {labsName} is the name of the community, such as Fluxer Labs.',
});

const SHOW_OPERATORS_LABEL_DESCRIPTOR = msg({
	message: 'Show #operators',
	comment:
		'Switch label in the Fluxer Labs section of the Operator Pass settings. On by default. #operators is a channel in the Fluxer Labs community, keep it as is.',
});
const SHOW_OPERATORS_HINT_DESCRIPTOR = msg({
	message:
		"Turning this off removes the roles that open #operators in {labsName}, and we won't add them back until you turn it on again.",
	comment:
		'Description under the switch "Show #operators" in the Operator Pass settings. #operators is a channel name, keep it as is. {labsName} is the name of the community, such as Fluxer Labs. Turning it off does not remove the account from the community, the pass or the badge.',
});

export const OperatorPassLabsGreetingSetting = observer(function OperatorPassLabsGreetingSetting() {
	const {i18n} = useLingui();
	const [saving, setSaving] = useState(false);
	const labs = OperatorPassHoldings.passes?.labs;
	if (!labs || !showsOperatorPassLabsSettings(Users.currentUser, OperatorPassHoldings.passes)) return null;
	const save = async (update: () => Promise<unknown>) => {
		setSaving(true);
		try {
			await update();
		} catch {
			ToastCommands.error(i18n._(WALL_SAVE_FAILED_DESCRIPTOR));
		} finally {
			setSaving(false);
		}
	};
	return (
		<SettingsSection
			id="operator-pass-labs"
			title={PRODUCT_LABS_COMMUNITY_NAME}
			description={i18n._(labs.member ? LABS_SECTION_DESCRIPTION_DESCRIPTOR : LABS_NOT_MEMBER_DESCRIPTION_DESCRIPTOR, {
				labsName: PRODUCT_LABS_COMMUNITY_NAME,
			})}
			data-flx="user.operator-pass-tab.operator-pass-labs-greeting-setting.section"
		>
			<Switch
				label={i18n._(SHOW_OPERATORS_LABEL_DESCRIPTOR)}
				description={i18n._(SHOW_OPERATORS_HINT_DESCRIPTOR, {labsName: PRODUCT_LABS_COMMUNITY_NAME})}
				value={labs.show_operators}
				onChange={(value) => void save(() => OperatorPassCommands.updateLabsOperators(value))}
				disabled={saving}
				data-flx="user.operator-pass-tab.operator-pass-labs-greeting-setting.switch.show-operators"
			/>
			{asksOperatorPassLabsGreeting() && (
				<Switch
					label={i18n._(LABS_GREETING_LABEL_DESCRIPTOR)}
					description={i18n._(LABS_GREETING_SETTING_HINT_DESCRIPTOR)}
					value={labs.greeting}
					onChange={(value) => void save(() => OperatorPassCommands.updateLabsGreeting(value))}
					disabled={saving}
					data-flx="user.operator-pass-tab.operator-pass-labs-greeting-setting.switch.greeting"
				/>
			)}
			{labs.show_operators && (
				<div className={styles.actions} data-flx="user.operator-pass-tab.operator-pass-labs-greeting-setting.actions">
					<OpenOperatorsButton
						variant="secondary"
						label={labs.member ? undefined : i18n._(JOIN_LABS_DESCRIPTOR, {labsName: PRODUCT_LABS_COMMUNITY_NAME})}
						data-flx="user.operator-pass-tab.operator-pass-labs-greeting-setting.button.open-operators"
					/>
				</div>
			)}
		</SettingsSection>
	);
});
