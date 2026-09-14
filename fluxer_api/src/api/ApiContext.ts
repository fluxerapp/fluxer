// SPDX-License-Identifier: AGPL-3.0-or-later

import type {InboundSmsChallengeService} from '@app/api/auth/services/InboundSmsChallengeService';
import type {PhoneAttemptRiskService} from '@app/api/auth/services/PhoneAttemptRiskService';
import type {IPhoneLookupRepository} from '@app/api/auth/services/PhoneLookupRepository';
import type {Config} from '@app/api/Config';
import type {IEmailDnsValidationService} from '@app/api/infrastructure/IEmailDnsValidationService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {IMediaService} from '@app/api/infrastructure/IMediaService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {BotMfaMirrorService} from '@app/api/oauth/BotMfaMirrorService';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import type {UserActivityBuffer} from '@app/api/user/services/UserActivityBuffer';
import type {UserContactChangeLogService} from '@app/api/user/services/UserContactChangeLogService';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import type {ISmsService} from '@pkgs/sms/src/ISmsService';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';

export interface ApiServices {
	users: IUserRepository;
	cache: ICacheService;
	gateway: IGatewayService;
	kv: IKVProvider;
	media: IMediaService;
	email: IEmailService;
	emailDnsValidation: IEmailDnsValidationService;
	sms: ISmsService;
	worker: IWorkerService<WorkerTaskName>;
	snowflake: ISnowflakeService;
	rateLimit: IRateLimitService;
	contactChangeLog: UserContactChangeLogService;
	inboundSmsChallenge: InboundSmsChallengeService | null;
	phoneLookup: IPhoneLookupRepository | null;
	phoneAttemptRisk: PhoneAttemptRiskService;
	botMfaMirror: BotMfaMirrorService;
	userActivityBuffer: UserActivityBuffer;
	config: typeof Config;
}

export interface RequestScope {
	requestId: string;
	clientIp: string | null;
	userAgent: string | null;
}

export interface ApiContext {
	services: ApiServices;
	request: RequestScope;
}
