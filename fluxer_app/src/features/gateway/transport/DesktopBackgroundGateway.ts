// SPDX-License-Identifier: AGPL-3.0-or-later

import {setAccountMentionCountSource} from '@app/features/auth/components/accounts/account_switcher_modal/AccountMentionCounts';
import BackgroundGatewaySessions from '@app/features/gateway/transport/BackgroundGatewayConnectionRegistry';

class DesktopBackgroundGateway {
	get totalMentionCount(): number {
		return BackgroundGatewaySessions.totalMentionCount;
	}

	getAccountMentionCount(accountKey: string): number {
		return BackgroundGatewaySessions.getAccountMentionCount(accountKey);
	}
}

const DesktopBackgroundGatewaySurface = new DesktopBackgroundGateway();

setAccountMentionCountSource(DesktopBackgroundGatewaySurface);

export default DesktopBackgroundGatewaySurface;
