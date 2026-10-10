// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {PremiumStateReconciliationQueueService} from '@app/api/infrastructure/PremiumStateReconciliationQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {
	getAppStoreServerApiClient,
	getGooglePlayDeveloperApiClient,
	getStoreBillingRepository,
} from '@app/api/middleware/ServiceSingletons';
import {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {StripeGiftReversalHandler} from '@app/api/stripe/services/StripeGiftReversalHandler';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

interface StoreEntitlementServiceFactoryParams {
	userRepository: UserRepository;
	userCacheService: UserCacheService;
	gatewayService: IGatewayService;
	kvClient: IKVProvider;
	snowflakeService: ISnowflakeService;
	premiumStateReconciliationQueueService: PremiumStateReconciliationQueueService;
}

export function createStoreEntitlementService(params: StoreEntitlementServiceFactoryParams): StoreEntitlementService {
	return new StoreEntitlementService({
		repository: getStoreBillingRepository(),
		userRepository: params.userRepository,
		userCacheService: params.userCacheService,
		gatewayService: params.gatewayService,
		kvClient: params.kvClient,
		snowflakeService: params.snowflakeService,
		appStoreClient: getAppStoreServerApiClient(),
		googlePlayClient: getGooglePlayDeveloperApiClient(),
		giftReversalHandler: new StripeGiftReversalHandler(
			params.userRepository,
			params.gatewayService,
			params.premiumStateReconciliationQueueService,
		),
	});
}
