! ===============================================================================
! Autor: Carlos Alberto Dias da S. Filho
! Email: filhotecmail@gmail.com
! Data de Criação: 2025-02-01
! Data de Revisão: 2025-02-01
! Sobre o programa: Este é um programa em Fortran F90 para
!   resolver o enunciado do Advent of Code 2025, dia 2.
! Objetivos: Resolver o problema do Dia 2 usando Fortran F90.
!
! Contexto matemático:
! - Um ID inválido é um número decimal cuja representação é a
!   concatenação de uma sequência de dígitos com ela mesma: S||S.
! - IDs inválidos têm número de dígitos par: 2k.
! - k = metade do comprimento e X o inteiro formado pelos
!   primeiros k dígitos - sem zeros à esquerda . Então:
!     N = X * 10^k + X = X * (10^k + 1).
! - Para um intervalo [L, R], e cada k em {1,2,3,4,5} (até 10 dígitos):
!     B_k = 10^k + 1,
!     X ∈ [ceil(L / B_k), floor(R / B_k)] ∩ [10^(k−1), 10^k − 1].
!   Os candidatos inválidos são N = X * B_k.
! - Essa forma evita varrer número a número, gera apenas N possíveis
!   diretamente via X e k.
! - Para intervalos sobrepostos, N deve ser deduplicado antes da soma.
! =================================================================================


!==============================================================================
! Módulo: aoc_day2_mod
! Finalidade: Define tipos e rotinas auxiliares e de processamento do Dia 2
! Conteúdo:
!   - Tipo interval_t: representa um intervalo [lo,hi]
!   - I/O: read_input_line (lê linha única da entrada)
!   - Parsing: parse_line (converte string em intervalos)
!   - Helpers: pow10, ceil_div, quicksort
!   - Processamento: compute_total (gera, deduplica e soma inválidos)
!   - Orquestração: run (fluxo principal)
!==============================================================================
module aoc_day2_mod
  implicit none
  ! Kind inteiro com ~64 bits para suportar IDs grandes
  integer, parameter :: ik = selected_int_kind(18)
  type interval_t
    ! Limites do intervalo [lo, hi]
    integer(ik) :: lo
    integer(ik) :: hi
  end type interval_t
contains

  !=============================================================================
  ! SUBROUTINE: read_input_line
  !
  ! DESCRIÇÃO:
  !   Realiza o encapsulamento da leitura de arquivo para extrair o conteúdo 
  !   bruto da primeira linha. Projetada especificamente para arquivos de 
  !   entrada única (single-line inputs) contendo strings de intervalos.
  !
  ! FLUXO DE EXECUÇÃO (Pipeline):
  !   1. Setup: Inicializa a string de saída para evitar lixo de memória.
  !   2. Abertura: Tenta abrir o arquivo no modo 'OLD' (deve existir previamente).
  !   3. Tratamento de Erro Silencioso: Se o arquivo não puder ser aberto (ios /= 0),
  !      a rotina retorna imediatamente, deixando 'line' vazia para o chamador tratar.
  !   4. Leitura: Utiliza o descritor de formato '(A)' para leitura alfanumérica.
  !   5. Cleanup: Fecha o descritor de unidade (Unit 10) para liberar o recurso.
  !
  ! PARÂMETROS:
  !   [in]  path : String com o caminho absoluto ou relativo do arquivo fonte.
  !   [out] line : Buffer que receberá o conteúdo lido (passado por referência).
  !
  ! CONSIDERAÇÕES DE ENGENHARIA:
  !   - Acoplamento: A unidade (Unit 10) é hardcoded, o que pode gerar conflitos 
  !     se outras partes do sistema usarem o mesmo ID simultaneamente.
  !   - Robustez: A rotina não valida se a string 'line' tem tamanho suficiente 
  !     para o conteúdo do arquivo (risco de truncamento se len(line) < len(file)).
  !=============================================================================
  !------------------------------------------------------------------------------
  ! Subroutine: read_input_line
  ! Propósito: Ler a primeira linha do arquivo de entrada contendo os intervalos
  ! Parâmetros:
  !   - path (in): caminho absoluto do arquivo
  !   - line (out): linha lida com todos os intervalos
  ! Observações: usa unidade fixa, retorna linha vazia se falhar ao abrir
  !------------------------------------------------------------------------------
  subroutine read_input_line(path, line)
    ! Lê a linha única de entrada contendo os intervalos
    ! path: caminho absoluto do arquivo de entrada
    ! line: string retornada com o conteúdo da primeira linha
    character(len=*), intent(in) :: path
    character(len=*), intent(out) :: line
    integer :: unit, ios
    unit = 10              ! Unidade de arquivo
    line = ''              ! Inicializa linha de saída
    open(unit=unit, file=path, status='old', iostat=ios) ! Abre arquivo existente
    if (ios /= 0) return   ! Falha ao abrir: retorna linha vazia
    read(unit,'(A)') line  ! Lê linha única contendo todos os intervalos
    close(unit)            ! Fecha arquivo
  end subroutine read_input_line

  !------------------------------------------------------------------------------
  ! Subroutine: parse_line
  ! Propósito: Converter a linha em intervalos [lo,hi] no formato "a-b"
  ! Parâmetros:
  !   - line (in): string com tokens separados por vírgula
  !   - intervals (out): vetor preenchido com os intervalos
  !   - count (out): quantidade de intervalos válidos
  ! Observações: ignora tokens vazios/malformados; limita ao tamanho do vetor
  !------------------------------------------------------------------------------
  subroutine parse_line(line, intervals, count)
    ! Converte a linha em tokens "a-b" separados por vírgula e
    ! preenche o vetor de intervalos [lo,hi]
    ! line: string com todos os intervalos
    ! intervals: saída com até size(intervals) intervalos
    ! count: quantidade efetiva preenchida
    character(len=*), intent(in) :: line
    type(interval_t), intent(out) :: intervals(:)
    integer, intent(out) :: count
    character(len=4096) :: s
    character(len=128) :: t, a, b
    integer :: pos, lenl, comma_pos, hypos, n
    integer(ik) :: lo, hi
    s = trim(line)         ! Normaliza espaços finais
    lenl = len_trim(s)
    n = 0
    pos = 1
    do                     ! Varre tokens separados por vírgula
      if (pos > lenl) exit
      comma_pos = index(s(pos:lenl), ',')
      if (comma_pos == 0) then
        t = trim(s(pos:lenl))
        pos = lenl + 1
      else
        t = trim(s(pos:pos+comma_pos-2))
        pos = pos + comma_pos
      end if
      if (len_trim(t) == 0) cycle
      hypos = index(t, '-') ! Divide token em "a-b"
      if (hypos == 0) cycle
      a = trim(t(1:hypos-1))
      b = trim(t(hypos+1:len_trim(t)))
      read(a, *) lo        ! Converte limites
      read(b, *) hi
      if (lo <= hi) then
        if (n < size(intervals)) then
          n = n + 1
          intervals(n)%lo = lo
          intervals(n)%hi = hi
        end if
      end if
    end do
    count = n              ! Retorna quantidade de intervalos parseados
  end subroutine parse_line

  !------------------------------------------------------------------------------
  ! Function: pow10
  ! Propósito: Calcular 10^k como inteiro(ik) por multiplicação iterativa
  ! Parâmetros:
  !   - k (in): expoente não-negativo
  ! Retorno:
  !   - p: 10^k
  !------------------------------------------------------------------------------
  pure function pow10(k) result(p)
    ! Calcula 10^k como inteiro(ik), sem recorrer a exponenciação
    ! k: expoente inteiro
    ! p: resultado 10^k
    integer, intent(in) :: k
    integer(ik) :: p
    integer :: i
    p = 1_ik
    do i = 1, k
      p = p * 10_ik
    end do
  end function pow10

  !------------------------------------------------------------------------------
  ! Function: ceil_div
  ! Propósito: Retornar ceil(a/b) para inteiros(ik), b>0
  ! Parâmetros:
  !   - a (in): numerador
  !   - b (in): denominador (>0)
  ! Retorno:
  !   - c: resultado arredondado para cima
  !------------------------------------------------------------------------------
  pure function ceil_div(a, b) result(c)
    ! Retorna ceil(a/b) para inteiros(ik), assumindo b>0
    ! a: numerador
    ! b: denominador
    ! c: resultado arredondado para cima
    integer(ik), intent(in) :: a, b
    integer(ik) :: c
    if (b <= 0_ik) then
      c = 0_ik
    else
      c = (a + b - 1_ik) / b
    end if
  end function ceil_div

  !------------------------------------------------------------------------------
  ! Subroutine: quicksort
  ! Propósito: Ordenar vetor de inteiros(ik) in-place (QuickSort clássico)
  ! Parâmetros:
  !   - a (inout): vetor a ordenar
  !   - l (in): índice inicial (1-based)
  !   - r (in): índice final (1-based)
  ! Observações: particiona por pivot e recorre nas metades
  !------------------------------------------------------------------------------
  recursive subroutine quicksort(a, l, r)
    ! Ordena vetor de inteiros(ik) in-place usando QuickSort
    ! a: vetor a ordenar
    ! l,r: índices de início e fim (1-based)
    integer(ik), intent(inout) :: a(:)
    integer, intent(in) :: l, r
    integer(ik) :: pivot, tmp
    integer :: i, j, m
    if (l >= r) return
    m = (l + r) / 2
    pivot = a(m)
    i = l
    j = r
    do
      do while (a(i) < pivot)
        i = i + 1
      end do
      do while (a(j) > pivot)
        j = j - 1
      end do
      if (i <= j) then
        tmp = a(i); a(i) = a(j); a(j) = tmp
        i = i + 1
        j = j - 1
      end if
      if (i > j) exit
    end do
    call quicksort(a, l, j)
    call quicksort(a, i, r)
  end subroutine quicksort

 !=============================================================================
  ! SUBROUTINE: compute_total
  !
  ! DESCRIÇÃO:
  !   Calcula o somatório de valores únicos (candidatos) baseados na relação 
  !   v = X * (10^k + 1). A rotina varre múltiplos intervalos de entrada e 
  !   identifica valores de X que, quando multiplicados pelo fator B_k, 
  !   caem dentro do intervalo fornecido e respeitam a janela de magnitude de k.
  !
  ! LÓGICA DE ENGENHARIA:
  !   1. Geração: Para cada ordem k [1, 5], define-se um multiplicador B_k.
  !      Os limites [xlo, xhi] são derivados por divisão inteira/teto, 
  !      garantindo que o produto resulte em um valor dentro do intervalo original.
  !   2. Clamping: Aplica-se restrição para que X pertença ao intervalo [10^k-1, 10^k-1].
  !   3. Coleta: Os candidatos são armazenados no array 'vals'. O tamanho é fixo 
  !      em 300.000 (max_vals) para evitar estouro de memória dinâmica excessiva.
  !   4. Processamento de Set: Como diferentes k ou intervalos podem gerar 
  !      colisões (mesmo valor final), o array é ordenado via Quicksort e 
  !      percorrido linearmente para somar apenas ocorrências únicas.
  !
  ! PARÂMETROS:
  !   [in]  intervals : Lista de estruturas de intervalo (lo, hi).
  !   [in]  count     : Número de intervalos válidos no array.
  !   [out] total     : Acumulador de 64-bit (ik) com o somatório dos únicos.
  !
  ! NOTA TÉCNICA:
  !   A complexidade temporal é O(k * count + N log N), onde N é o total de 
  !   candidatos gerados. O limite max_vals é uma restrição de design; 
  !   valores excedentes são ignorados silenciosamente para manter estabilidade.
  !=============================================================================
  !------------------------------------------------------------------------------
  ! Subroutine: compute_total
  ! Propósito: Gerar N = X*(10^k+1) nos intervalos, deduplicar e somar
  ! Parâmetros:
  !   - intervals (in): vetor de intervalos [lo,hi]
  !   - count (in): quantidade de intervalos válidos
  !   - total (out): soma dos IDs inválidos únicos
  ! Observações: usa ordenação para deduplicação; limita memória por max_vals
  !------------------------------------------------------------------------------
  subroutine compute_total(intervals, count, total)
    ! Gera todos os candidatos inválidos N = X*(10^k+1) nos intervalos
    ! Deduplica e soma os valores únicos
    ! intervals,count: entrada de intervalos
    ! total: soma dos inválidos únicos
    type(interval_t), intent(in) :: intervals(:)
    integer, intent(in) :: count
    integer(ik), intent(out) :: total
    integer :: k, idx
    integer :: unique_count
    integer(ik) :: i
    integer(ik) :: bk, xlo, xhi, lo_k, hi_k
    integer, parameter :: max_vals = 300000
    integer(ik), allocatable :: vals(:)
    integer :: nvals
    allocate(vals(max_vals))
    nvals = 0
    do k = 1, 5
      bk = pow10(k) + 1_ik
      lo_k = pow10(k-1)
      hi_k = pow10(k) - 1_ik
      print *, 'Processando k = ', k, ' com B_k = ', bk
      do idx = 1, count
        xlo = ceil_div(intervals(idx)%lo, bk)
        xhi = intervals(idx)%hi / bk
        if (xlo < lo_k) xlo = lo_k
        if (xhi > hi_k) xhi = hi_k
        if (xlo <= xhi) then
          print *, 'Intervalo ', idx, ' [', intervals(idx)%lo, '-', intervals(idx)%hi, '] => X em [', xlo, ',', xhi, ']'
          do i = 0_ik, xhi - xlo
            if (nvals < max_vals) then
              nvals = nvals + 1
              vals(nvals) = (xlo + i) * bk
              print *, 'Par repetido encontrado: ', vals(nvals)
            end if
          end do
        else
          print *, 'Intervalo ', idx, ' não gera candidatos para k = ', k
        end if
      end do
    end do
    print *, 'Deduplicando e somando pares repetidos...'
    if (nvals > 1) call quicksort(vals, 1, nvals)
    total = 0_ik
    unique_count = 0
    do i = 1_ik, nvals
      if (i == 1_ik .or. vals(i) /= vals(i-1)) then
        total = total + vals(i)
        unique_count = unique_count + 1
      end if
    end do
    print *, 'Total de pares únicos: ', unique_count
    deallocate(vals)
  end subroutine compute_total

  subroutine run
    ! Fluxo principal: leitura, parsing, geração e soma, impressão
    character(len=4096) :: input_line
    type(interval_t) :: intervals(1024)
    integer :: count
    integer :: ii
    integer(ik) :: total
    ! Caminho absoluto para a entrada com uma linha de intervalos
    print *, 'Lendo entradas...'
    call read_input_line('c:\projetos de estudos\AdventofCode\202502\Enunciado do problema\Imput de dados par analise.txt', input_line)
    ! Converte a linha em vetor de intervalos [lo,hi]
    print *, 'Analisando entradas...'
    call parse_line(input_line, intervals, count)
    print *, 'Total de intervalos lidos: ', count
    if (count > 0) then
      do ii = 1, count
        print *, 'Intervalo ', ii, ': ', intervals(ii)%lo, '-', intervals(ii)%hi
      end do
    end if
    print *, 'Iniciando geração de pares repetidos e soma...'
    call compute_total(intervals, count, total)
    print *, 'A Soma dos Pares repetidos: ', total
  end subroutine run
end module aoc_day2_mod

program aoc_day2
  use aoc_day2_mod
  implicit none
  call run
end program aoc_day2
