# CLAUDE.md

이 파일은 Claude Code가 이 레포지토리에서 작업할 때 참고하는 가이드라인입니다.

## Repository Summary

이 레포지토리는 Alex Xu의 "System Design Interview" 책을 스터디하면서 실습해보고 싶은 시스템 디자인 개념들을 하나씩 구현해보는 공간입니다.  
각 실습은 독립된 디렉토리(예: `01-rate-limiter/`)에 있으며, Pulumi와 AWS를 사용해 실제 인프라로 구현합니다.

## Architecture Note

- 각 실습은 독립된 디렉토리에 격리되어 있으며 독립적인 의존성을 가집니다
- 모든 인프라는 Pulumi의 TypeScript SDK를 사용해 선언적으로 정의됩니다
- `@pulumi/aws`와 `@pulumi/awsx` 패키지를 사용해 리소스를 프로비저닝합니다
- 각 실습은 시스템 디자인 패턴을 실제 클라우드 인프라로 구현하여 학습합니다
