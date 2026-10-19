---
task: shared/README.md 초안 작성 (사용자가 research/shared/README.md를 쓸 때 참고할 초안)
depends_on: []
status: done
---

# shared/README.md 초안 — slca

## 패키지 개요
- `slca` 1.4.1 (DESCRIPTION 기준, 소스 트리 버전). 저자/관리자 Youngsun Kim, 공저 Hwan Chung. GPL (>= 3), Rcpp 컴파일 포함.
- CRAN 등재 패키지 (README 배지, `install.packages("slca")`). 1.4.1이 CRAN 반영본인지는 저장소 안에 증거 없음 — cran-comments.md는 제출 맥락 기록일 뿐 수락 증명 아님 (maintenance/README.md 명시).
- 모형 범위: Structural Latent Class Model — 복수 잠재범주변수 간 구조모형. LTA (Collins & Lanza 2009), LCPA (Chung et al. 2010), JLCA (Jeon et al. 2017) 및 확장. 수식 문법 `L1(3) ~ x1 + x2`, 구조 `L1 ~ L2`, 상위변수 `P[4] ~ L1 + L2 + L3`, `constraints=`로 측정불변성/전이확률 동질성 제약.
- 범주형 지표만 지원. 연속형 지표 미지원 (issue #1 maintainer clarification).
- 코드: R/ 17개 파일 (arguments, data_encode, estimate/estModel, regress, simulate, diagnostic, plot, methods 등) + src/ C++. 테스트: tests/testthat/test-slca.R 단일 파일. 의존: DiagrammeR, magrittr, MASS, Rcpp, stats; Suggests testthat (>= 3.0.0).

## 현재 유지보수 상태 (maintenance/ 기준)
- 저장소 성격: 유지보수 전용. 연구 폴더(research/)는 2026-10-17~19 신설, 노트 없음 (claude/README.md 빈 틀).
- Baseline: public master `18f16f6b…` (2026-10-16 검토). R CMD check·pkgdown CI 모두 성공 (해당 SHA 한정). 로컬 `load_all()` + `test_local()` R 4.5.0 통과.
- CI matrix: macOS/Windows R release, Ubuntu release·devel·oldrel-1 (.github/workflows/R-CMD-check.yaml). pkgdown 별도 (문서 빌드 성공 ≠ 정확성).
- cran-comments.md: 0 errors / 1 warning (Apple clang `-Wfixed-enum-extension`, R 헤더 유발) / 1 note (HTML Tidy 구버전). 과거 기록이며 현 SHA의 신규 체크 아님.
- Open issues: 없음 (baseline 시점). #1 closed (2026-03-30): 제약된 지표들의 관측 수준 불일치 → `arg_mf` 오류 "levels for variables constrained to equal are different". 원인 소스 확인 완료(R/data_encode.R `proc_data`, R/arguments.R `arg_mf`), 결정적 재현 스크립트 baseline 문서에 있음. 코드 변경 없음, 전용 회귀 테스트 없음.
- NEWS 1.4.1: 결측 처리·응답 인코딩(-1, 0-based) 수정, `predict.slcafit()` newdata 생략 수정, 입력 검증 추가, Hessian 공분산·회귀 SE·Wald p 수정(`summary.reg.slca()`), BCH/ML 3단계 bias-adjusted 내부 수정, bootstrap 실패 기록화.
- 최근 커밋: 40d7d79 maintenance 워크플로우 수립 → fc157c4/b3ae6cc/c3b36ea/85f1eb2 research 프로토콜 scaffold. 작업트리 깨끗함.

## 확정된 방침 vs 제안 상태인 것
확정 (maintenance/README.md, 커밋됨):
- 이슈 → 최소 재현 → 원인/수정 → 회귀 테스트 → 릴리스 후보의 5단계. 가설은 가설로 표기, closed ≠ fixed.
- 버전 bump·CRAN 제출·태그·릴리스는 관리자(사용자) 별도 결정. 문서 전용 변경은 버전/테스트 변경 불요.
- 개인 자료·미공개 데이터 저장소 반입 금지, 합성/공개 예시만.
- maintenance/는 `^maintenance$`로 빌드 제외. research/는 .Rbuildignore 처리 여부 미확인 [추정: 미처리 → 소스 아카이브에 포함될 가능성].
- 연구 기록은 PROTOCOL.md: shared/는 사용자 전용, 에이전트는 각자 폴더에만 기록, 제안은 자기 README 제안 절.
제안 상태 (baseline "Next actions", 미채택):
- #1 전용 회귀 테스트 추가 (수준 수 같고 라벨/순서 다른 케이스 포함).
- 명시적 후보 SHA 선정 후 전체 check matrix 재실행, cran-comments와 warning/note 대조.
- 연속형 지표 지원은 기능 결정 사안 — 의미·검증 먼저 정의, 버그 수정 아님.

## 미해결·막힌 것
- 1.4.1의 CRAN 반영 여부·날짜 저장소 내 미기록.
- 현 HEAD(85f1eb2)에 대한 R CMD check/테스트 결과 없음 (문서 변경만이라 baseline 결과로 갈음; 신규 실행은 미수행).
- Apple clang 경고는 R 헤더 기인이라 패키지 측 해결 불가 [추정: CRAN incoming에서 플랫폼별 재현 여부 미확인].
- 테스트 단일 파일 집중(test-slca.R) — 커버리지 수치 없음.
- 공개 이슈 목록은 2026-10-16 스냅샷; 재확인 필요.
- 이 세션에서 R 미실행, 모든 상태는 문서·git 기준.

## 후보 과제 (유지보수·검증 성격)
1. #1 회귀 테스트 추가 — baseline이 지정한 첫 과제, 재현 스크립트 이미 존재해 비용 낮음.
2. 1.4.1 NEWS 항목별 테스트 대응표 작성 — 어떤 수정이 test-slca.R에서 보장되는지 확인, 빈칸은 테스트 보강 후보.
3. 현 HEAD 기준 전체 check matrix 재실행 + cran-comments.md 갱신 — 릴리스 게이트 증거 최신화.
4. research/ 폴더 .Rbuildignore 처리 확인·추가 — 소스 아카이브에 연구 노트 유출 방지 [추정 전제].
5. 3단계 회귀(BCH/ML) SE·CI 수치 검증 — 1.4.1에서 수정된 부분이라 합성 데이터 시뮬레이션으로 불편성·커버리지 점검, 추후 논문용 근거로도 재사용 가능.

## 출처 파일 목록
- research/PROTOCOL.md, research/claude/README.md, CLAUDE.md
- README.md, DESCRIPTION, NEWS.md, cran-comments.md
- maintenance/README.md, maintenance/2026-10-16-baseline.md
- 파일 목록: R/, tests/testthat/test-slca.R, .github/workflows/{R-CMD-check,pkgdown}.yaml
- git log (85f1eb2 … 40d7d79), git status
